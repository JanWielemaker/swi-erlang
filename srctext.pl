:- module(srctext,
          [ with_source/2                       % :Goal, +Options
          ]).
:- use_module(library(modules)).
:- use_module(library(settings)).
:- use_module(library(option)).
:- use_module(library(apply)).
:- use_module(library(http/http_open)).

:- meta_predicate
    with_source(0, +).

:- multifile
    prepare_module/3,               % +Module, +Application, +Options
    prepare_goal/3.                 % +GoalIn, -GoalOut, +Options

%!  with_source(:Goal, +Options)
%
%   Run Goal in an isolated module.  Options:
%
%     - module(?Module)
%       Use Module for the isolation.  If unbound it will be bound
%       to a random name.
%     - application(+Application)
%       Module to inherit from.  Default is `user`.

with_source(Goal, Options) :-
    option(application(Application), Options, user),
    option(module(Module), Options, _),
    in_temporary_module(
        Module,
        pengine_prepare_source(Application, Options),
        Goal).

%!  pengine_prepare_source(:Application, +Options) is det.
%
%   Load the source into the pengine's module.
%
%   @throws =prepare_source_failed= if it failed to prepare the
%           sources.

pengine_prepare_source(Module:Application, Options) :-
    setting(Application:program_space, SpaceLimit),
    set_module(Module:program_space(SpaceLimit)),
    delete_import_module(Module, user),
    add_import_module(Module, Application, start),
    prep_module(Module, Application, Options).

prep_module(Module, Application, Options) :-
    maplist(copy_flag(Module, Application), [var_prefix]),
    forall(prepare_module(Module, Application, Options), true),
    setup_call_cleanup(
        '$set_source_module'(OldModule, Module),
        maplist(process_create_option(Module), Options),
        '$set_source_module'(OldModule)).

copy_flag(Module, Application, Flag) :-
    current_prolog_flag(Application:Flag, Value),
    !,
    set_prolog_flag(Module:Flag, Value).
copy_flag(_, _, _).

process_create_option(Application, src_text(Text)) :-
    !,
    pengine_src_text(Text, Application).
process_create_option(Application, src_url(URL)) :-
    !,
    pengine_src_url(URL, Application).
process_create_option(_, _).


                 /*******************************
                 *        COMPILE SOURCE        *
                 *******************************/

/** pengine_src_text(+SrcText, +Module) is det

Asserts the clauses defined in SrcText in   the  private database of the
current Pengine. This  predicate  processes   the  `src_text'  option of
pengine_create/1.
*/

pengine_src_text(Src, Module) :-
    pengine_self(Self),
    format(atom(ID), 'pengine://~w/src', [Self]),
    extra_load_options(Self, Options),
    setup_call_cleanup(
        open_chars_stream(Src, Stream),
        load_files(Module:ID,
                   [ stream(Stream),
                     module(Module),
                     silent(true)
                   | Options
                   ]),
        close(Stream)),
    keep_source(Self, ID, Src).

system:'#file'(File, _Line) :-
    prolog_load_context(stream, Stream),
    set_stream(Stream, file_name(File)),
    set_stream(Stream, record_position(false)),
    set_stream(Stream, record_position(true)).

%%   pengine_src_url(+URL, +Module) is det
%
%    Asserts the clauses defined in URL in   the private database of the
%    current Pengine. This predicate processes   the `src_url' option of
%    pengine_create/1.
%
%    @tbd: make a sensible guess at the encoding.

pengine_src_url(URL, Module) :-
    pengine_self(Self),
    uri_encoded(path, URL, Path),
    format(atom(ID), 'pengine://~w/url/~w', [Self, Path]),
    extra_load_options(Self, Options),
    (   get_pengine_application(Self, Application),
        setting(Application:debug_info, false)
    ->  setup_call_cleanup(
            http_open(URL, Stream, []),
            ( set_stream(Stream, encoding(utf8)),
              load_files(Module:ID,
                         [ stream(Stream),
                           module(Module)
                         | Options
                         ])
            ),
            close(Stream))
    ;   setup_call_cleanup(
            http_open(URL, TempStream, []),
            ( set_stream(TempStream, encoding(utf8)),
              read_string(TempStream, _, Src)
            ),
            close(TempStream)),
        setup_call_cleanup(
            open_chars_stream(Src, Stream),
            load_files(Module:ID,
                       [ stream(Stream),
                         module(Module)
                       | Options
                       ]),
            close(Stream)),
        keep_source(Self, ID, Src)
    ).


extra_load_options(Pengine, Options) :-
    pengine_not_sandboxed(Pengine),
    !,
    Options = [].
extra_load_options(_, [sandboxed(true)]).


keep_source(Pengine, ID, SrcText) :-
    get_pengine_application(Pengine, Application),
    setting(Application:debug_info, true),
    !,
    to_string(SrcText, SrcString),
    assertz(pengine_data(Pengine, source(ID, SrcString))).
keep_source(_, _, _).

to_string(String, String) :-
    string(String),
    !.
to_string(Atom, String) :-
    atom_string(Atom, String),
    !.
