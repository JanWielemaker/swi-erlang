/*  Part of SWI-Prolog

    Author:        Torbjörn Lager and Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(pengines,  
          [ flush/0,                            % From actors
            pengine_spawn/1,                    % -Pid
            pengine_spawn/2,                    % -Pid, +Options
            pengine_ask/2,                      % +Pid, :Query
            pengine_ask/3,                      % +Pid, :Query, +Options
            pengine_next/1,                     % +Pid
            pengine_next/2,                     % +Pid, +Options
            pengine_stop/1,                     % +Pid                   
            pengine_stop/2,                     % +Pid, +Options                   
            pengine_abort/1,                    % +Pid    
            pengine_input/2,                    % +Prompt, ?Answer
            pengine_respond/2,                  % +Pid, +Answer
            pengine_output/1,                    % +Term
            
            out/1
          ]).

:- use_module(library(broadcast)).
:- use_module(library(http/websocket)). % TODO: This should go

:- use_module(actors).
:- use_module(dollar_expansion).
:- use_module(format).

:- use_module(library(debug)).


:- op(400, fx, debugg).

debugg(Goal) :-
    debug(ws, 'CALL ~p', [Goal]),
    call(Goal),
    debug(ws, 'EXIT ~p', [Goal]).

:- meta_predicate 
    session(:, +, +).
    

:- dynamic
    reply_to/2,
    pengine_target/2,                 % Id, Target
    target_socket_format/3.           % Target, Socket, Format


pengines_node_action(pengine_spawn, Data, WebSocket) :-
    _{options:OptionString} :< Data,
    !,
    term_string(Options, OptionString),
    option(reply_to(ReplyTo), Options),
    select_option(format(Format), Options, RestOptions, 'json-s'),
    pengine_spawn(Pid, [sandboxed(false)|RestOptions]),
    assertz(target_socket_format(ReplyTo, WebSocket, Format)),
    thread_self(Self),
    assertz(reply_to(thread(Self), ReplyTo)),
    term_string(Pid, PidString),
    send(ReplyTo, spawned(PidString)).
pengines_node_action(pengine_ask, Data, WebSocket) :-
    _{pid:PidString, goal:GoalString, options:OptionString} :< Data,
    !,
    read_term_from_atom(GoalString, Goal, [variable_names(Bindings)]),    
    term_string(Options, OptionString),
    term_string(Pid, PidString),
    pengine_target(Pid, Target),
    target_socket_format(Target, WebSocket, Format),
    fix_template(Format, Goal, Bindings, NewTemplate),
    pengine_ask(Pid, Goal, [template(NewTemplate)|Options]).
pengines_node_action(pengine_next, Data, _WebSocket) :-
    _{pid:PidString, options:OptionString} :< Data,
    !,
    term_string(Options, OptionString),
    term_string(Pid, PidString),
    pengine_next(Pid, Options).    
pengines_node_action(pengine_stop, Data, _WebSocket) :-
    _{pid:PidString, options:OptionString} :< Data,
    !,
    term_string(Options, OptionString),
    term_string(Pid, PidString),
    pengine_stop(Pid, Options).
pengines_node_action(pengine_respond, Data, _WebSocket) :-
    _{pid:PidString, prolog:String} :< Data,
    !,
    term_string(Term, String),
    term_string(Pid, PidString),
    pengine_respond(Pid, Term).
pengines_node_action(pengine_abort, Data, _WebSocket) :-
    _{pid:PidString} :< Data,
    !,
    term_string(Pid, PidString),
    pengine_abort(Pid).



pengines_send_remote(Target, Message) :-
    target_socket_format(Target, Socket, Format),
    !,
    answer_format(Message, Json, Format),
    ws_send(Socket, json(Json)).  % TODO: This should not be here


:- dynamic
    child_parent/2.

:- listen(actor(spawned, Local, Pid),
    (   debug(listen, 'Actor ~p spawned actor ~p.', [Local,Pid]),
        assertz(child_parent(Pid, Local))
    )).

:- listen(actor(down, Pid),
    (   debug(listen, 'Actor ~p is down.', [Pid]),
        sleep(0.1), % TODO: This points to a bug!
        retractall(child_parent(Pid, _))
    )).


%!  out(+Term) is det.
%
%   Send Term to the shell.

out(Term) :-
    thread_self(Self), 
    root(Self, Root),
    reply_to(Root, Target),
    !,
    Target ! output(Self, Term).
out(_Term).  % This happens when there is no root connected to a shell.


root(Child, thread(Thread)) :-
    child_parent(Child, thread(Thread)),
    !.
root(Child, GrandParent) :-
    child_parent(Child, Parent),
    root(Parent, GrandParent).
    
    


%!  pengine_spawn(-Pid) is det.
%!  pengine_spawn(-Pid, +Options) is det.
%
%   Spawn a new pengine.  Options:
%
%     - exit(+Bool)
%       Determines if the pengine session must exit after having run 
%       a query to completion. Defaults to false.
%     - reply_to(+Target)
%       Send messages to Target. Default is the thread or engine that
%       called pengine_spawn/1-2.
    
    
pengine_spawn(Pid) :-
    pengine_spawn(Pid, []).

pengine_spawn(Pid, Options) :-
    self(Self),
    option(reply_to(Target), Options, Self),
    option(exit(Exit), Options, false),
    spawn(session(Pid, Target, Exit), Pid, [
          application(pengines)
        | Options
    ]),
    asserta(pengine_target(Pid, Target)).


:- thread_local parent/1.

session(Pid, Parent, Exit) :-
    assertz(parent(Parent)),
    session2(Pid, Parent, Exit).

session2(Pid, Parent, Exit) :-
    catch(guarded_session(Pid, Parent, Exit), exit_query, (
            Parent ! abort(Pid),
            session2(Pid, Parent, Exit)
        )
    ).

guarded_session(Module:Pid, Parent, Exit) :-
    receive({
        pengine:ask(Goal, Options) ->
            ask(Module:Goal, Pid, Parent, Options)
    }),
    (   Exit == true
    ->  true
    ;   guarded_session(Module:Pid, Parent, Exit)
    ).


% TODO: Currently only works when sandboxed = false, probably due
% to offset/2 not being made and declared as safe.

ask(Goal0, Self, Parent, Options) :-
    strip_module(Goal0, M, Goal1),
    option(template(Template0), Options, Goal1),
    maybe_expand(Goal1, Goal, Template0, Template), % FIXME? Hack - see below!    
    option(offset(Offset), Options, 0),
    option(limit(Limit), Options, 1),
    option(reply_to(ReplyTo), Options, Parent),
    State = count(Limit),
    OutPut = replyto(ReplyTo),
    (   call_cleanup(findn0(State, Template, M:offset(Offset, Goal), Solutions, Error), Det=true),
        (   var(Error),
            arg(1, OutPut, Out)
        ->  (   var(Det)
            ->  Out ! success(Self, Solutions, true),
                receive({
                    pengine:next(From, Count) -> 
                        nb_setarg(1, OutPut, From),
                        nb_setarg(1, State, Count),
                        fail;
                    pengine:stop(From) ->
                        From ! stop(Self)  
                })
            ;   Out ! success(Self, Solutions, false)
            )
        ;   ReplyTo ! error(Self, Error)
        )
    ;   ReplyTo ! failure(Self)
    ).


/* It seems that since toplevel variables are stored as thread_local
   we cannot expand variables earlier than here. It would be nice if 
   this could be done earlier.
*/

maybe_expand(Goal0, Goal, Template0, Template) :-
    is_dict(Template0), !,
    dict_pairs(Template0, Tag, Pairs0),
    wp_expand_query(Goal0, Goal, Pairs0, Pairs),
    dict_pairs(Template, Tag, Pairs).
maybe_expand(Goal, Goal, Template, Template). 


        
findn0(State, Template, Goal, Solutions, Error) :-
    catch(findn(State, Template, Goal, Solutions), Error, true).
    
findn(N, Template, Goal, Solutions) :- 
    findnsols(N, Template, Goal, Solutions), 
    Solutions \== [].


%!  pengine_ask(+Pid, :Query) is det.
%!  pengine_ask(+Pid, :Query, +Options) is det.
%
%   Call pengine Pid with Query. Options:
%
%     - template(+Template)
%       Template is a variable (or a term containing variables) 
%       shared with Query. By default, the template is identical to
%       Query.
%     - offset(+Integer)
%       Retrieve the slice of solutions to Query starting from Integer.
%       Default is 0.
%     - limit(+Integer)
%       Integer indicates the maximum number of solutions to retrieve
%       in one batch. A value of 1 means a unary list (default).
%     - reply_to(+Target)
%       Send messages to Target. Default is the thread or engine that
%       called pengine_spawn/1-2.

pengine_ask(Pid, Goal) :-
    pengine_ask(Pid, Goal, []).

pengine_ask(Pid, Goal, Options) :-
    Pid ! pengine:ask(Goal, Options). 
    
        
%!  pengine_next(+Pid) is det.
%!  pengine_next(+Pid, +Options) is det.
%
%   Ask pengine Pid for more solutions to Query. Options:
%
%     - limit(+Integer)
%       Integer indicates the maximum number of solutions to retrieve
%       in one batch. A value of 1 means a unary list (default).
%     - reply_to(+Target)
%       Send messages to Target. Default is the thread or engine that
%       called pengine_spawn/1-2.

pengine_next(Pid) :-
    pengine_next(Pid, []).

pengine_next(Pid, Options) :-
    self(Self),
    option(limit(Limit), Options, 1),
    option(reply_to(ReplyTo), Options, Self),
    Pid ! pengine:next(ReplyTo, Limit).


%!  pengine_stop(+Pid) is det.
%
%   Tell pengine Pid to stop. If successful, delivers a message
%   `stop(Pid)` to the mailbox of the process that called 
%   pengine_spawn/2-3. Options:
%
%     - reply_to(+Target)
%       Send `stop' message to Target. Default is the thread or
%       engine that called pengine_spawn/1-2.

pengine_stop(Pid) :-
    pengine_stop(Pid, []).
    
pengine_stop(Pid, Options) :-
    self(Self),
    option(reply_to(ReplyTo), Options, Self),
    Pid ! pengine:stop(ReplyTo). 
    
    
%!  pengine_output(+Term) is det.
%
%   Send Term to the parent process.

pengine_output(Term) :-
    self(Self), 
    parent(Parent),
    Parent ! output(Self, Term).


%!  pengine_input(+Prompt, -Input) is det.
%
%   Send Prompt to the parent process and wait for input. Prompt may
%   be any term, compound or atomic.
%
%   @bug: Why does Parent and _Parent not unify in the remote case?

pengine_input(Prompt, Input) :-
    self(Self),
    parent(Parent),
    Parent ! prompt(Self, Prompt),
    receive({ 
        input(_Parent, Input) ->
            true
    }).


%!  pengine_respond(+Pid, +Input) is det.
%
%   Send a response in the form of a term Input to a pengine Pid that
%   has prompted its parent process for input.

pengine_respond(Pid, Term) :-
    self(Self),
    Pid ! input(Self, Term).
    

%!  pengine_abort(+Pid) is det.
%
%   Tell pengine Pid to abort any query that it currently runs. If
%   successful, delivers a message `abort(Pid)' to the mailbox of the
%   process that called pengine_spawn/2-3.

pengine_abort(Pid) :-
    catch(thread_signal(Pid, throw(exit_query)), _, true).

  
		 /*******************************
		 *    EXTEND LOCAL PROCESSES	*
		 *******************************/

:- multifile
    hook_node_action/3.
    
distribution:hook_node_action(Actions, Data, WebSocket) :-
    pengines_node_action(Actions, Data, WebSocket).
    
distribution:hook_send_remote(Target, Message) :-
    pengines_send_remote(Target, Message).

