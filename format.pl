:- module(format, 
        [ fix_template/5,
          answer_format/3
        ]).

:- use_module(library(http/http_json)).
:- use_module(library(term_to_json)).
:- use_module(library(apply)).

:- use_module(library(debug)).


%!  fix_template(+Format, :Goal, :Goal, +Bindings, -NewTemplate) is det.
%

fix_template(Format, Goal, Goal, Bindings, NewTemplate) :-
    json_lang(Format),
    !,    
    exclude(anon, Bindings, NamedBindings),
    dict_create(NewTemplate, json, NamedBindings).
fix_template(_, Template, _, _, Template).


%!  json_lang(+Format) is semidet.
%
%   True if Format is a JSON variation.

json_lang(json) :- !.
json_lang(Format) :-
    sub_atom(Format, 0, _, _, 'json-').
    
    
anon(Name=_) :-
    sub_atom(Name, 0, _, _, '_'),
    sub_atom(Name, 1, 1, _, Next),
    char_type(Next, prolog_var_start).
    
    

%!  answer_format(+PrologMessage, -JsonMessage, +Format) is det.
%

answer_format(spawned(Pid),
              json{type:spawned, pid:Pid},
              json) :- !.
answer_format(success(Pid, Bindings0, More),
              json{type:success, pid:Pid, data:Bindings, more:More},
              json) :- !,
    term_to_json(Bindings0, Bindings).
answer_format(failure(Pid),
              json{type:failure, pid:Pid},
              json) :- !.
answer_format(stop(Pid),
              json{type:stop, pid:Pid},
              json) :- !.
answer_format(error(Pid, ErrorTerm),
              json{type:error, pid:Pid, data:Message},
              json) :- !,
    message_to_string(ErrorTerm, Message).
answer_format(prompt(Pid, Term),
              json{type:prompt, pid:Pid, data:Term},
              json) :- !.
answer_format(output(Pid, Term),
              json{type:output, pid:Pid, data:Term},
              json) :- !.
answer_format(down(Pid, ErrorTerm),
              json{type:down, pid:Pid, data:Message},
              json) :- !,
    message_to_string(ErrorTerm, Message).
    
answer_format(spawned(Pid),
              json{type:spawned, pid:Pid},
              'json-s') :- !.
/*              
answer_format(success(ID, Answers0, More), JSON,
              'json-s') :- !,
    JSON = json{type:success, pid:ID, data:Answers, more:More},
    maplist(wp_expand_answer, Answers0, Answers1),
    maplist(answer_to_json_strings, Answers1, Answers).
*/
answer_format(success(ID, Answers0, More), JSON,
              'json-s') :- !,
    JSON = json{type:success, pid:xxx, data:Answers, more:More},
    maplist(answer_to_json_strings, Answers0, Answers).              
answer_format(failure(Pid),
              json{type:failure, pid:xxx},
              'json-s') :- !.
answer_format(stop(Pid),
              json{type:stop, pid:xxx},
              'json-s') :- !.
answer_format(error(Pid, ErrorTerm),
              json{type:error, pid:xxx, data:Message},
              'json-s') :- !,
    message_to_string(ErrorTerm, Message).
answer_format(prompt(Pid, Term),
              json{type:prompt, pid:xxx, data:Term},
              'json-s') :- !.
answer_format(output(Pid, Term),
              json{type:output, pid:xxx, data:JSON},
              'json-s') :- !,
    map_output(Term, JSON).
answer_format(down(Pid, ErrorTerm),
              json{type:down, pid:xxx, data:Message},
              'json-s') :- !,
    message_to_string(ErrorTerm, Message).
              
answer_format(Answer, Answer, prolog).
    

map_output(Term, Data) :-
    (   atomic(Term)
    ->  Data = Term
    ;   is_dict(Term, json),
        ground(json)        % TBD: Check proper JSON object?
    ->  Data = Term
    ;   term_string(Term, Data)
    ). 
    
%%    answer_to_json_strings(+AnswerDictIn, -AnswerDict).
%
%    Translate answer dict with Prolog term   values into answer dict
%    with string values.

answer_to_json_strings(DictIn, DictOut) :-
    dict_pairs(DictIn, Tag, Pairs),
    maplist(term_string_value, Pairs, BindingsOut),
    dict_pairs(DictOut, Tag, BindingsOut).

term_string_value(N-V, N-A) :-
    with_output_to(string(A),write_term(V,
                  [ quoted(true)
                  ])).

