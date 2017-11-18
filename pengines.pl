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
          [ flush/0,                            % From dispatch
            pengine_spawn/1,                    % -Pid
            pengine_spawn/2,                    % -Pid, +Options
            pengine_ask/2,                      % +Pid, +Query
            pengine_ask/3,                      % +Pid, +Query, +Options
            pengine_next/1,                     % +Pid
            pengine_next/2,                     % +Pid, +Options
            pengine_stop/1,                     % +Pid                   
            pengine_abort/1,                    % +Pid    
            pengine_input/2,                    % +Prompt, ?Answer
            pengine_respond/2,                  % +Pid, +Answer
            pengine_output/1                    % +Term
          ]).

:- use_module(dispatch).

:- meta_predicate 
    session(:, +, +).


%!  pengine_spawn(-Pid) is det.
%!  pengine_spawn(-Pid, +Options) is det.
%
%   Spawn a new pengine.  Options:
%
%     - exit(+Bool)
%       Determines if the pengine session must exit after having run 
%       a query to completion. Defaults to false.

pengine_spawn(Pid) :-
    pengine_spawn(Pid, []).

pengine_spawn(Pid, Options) :-
    self(Self),
    option(reply_to(Target), Options, Self),
    option(exit(Exit), Options, false),
    spawn(session(Pid, Target, Exit), Pid, [
          application(pengines)
        | Options
    ]).


:- thread_local parent/1.

session(Pid, Parent, Exit) :-
    assertz(parent(Parent)),
    session2(Pid, Parent, Exit).

session2(Pid, Parent, Exit) :-
    catch(guarded_session(Pid, Parent, Exit), Exception, true),
    (   Exception == exit_query
    ->  Parent ! abort(Pid),
        session2(Pid, Parent, Exit)
    ;   nonvar(Exception)
    ->  throw(Exception)
    ;   true
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
    

ask(Goal, Self, Parent, Options) :-
% TODO: Currently only works when sandboxed = false, probably due
% to offset/2 not being made and declared as safe.

ask(Goal0, Self, Parent, Options) :-
    strip_module(Goal0, M, Goal),
    option(template(Template), Options, Goal),
    option(offset(Offset), Options, 0),
    option(limit(Limit), Options, 1),
    State = count(Limit),
    (   call_cleanup(findn0(State, Template, M:offset(Offset, Goal), Solutions, Error), Det=true),
        (   var(Error)
        ->  (   var(Det)
            ->  Parent ! success(Self, Solutions, true),
                receive({
                    pengine:next(Count) -> 
                        nb_setarg(1, State, Count),
                        fail;
                    pengine:stop ->
                        Parent ! stop(Self)  
                })
            ;   Parent ! success(Self, Solutions, false)
            )
        ;   Parent ! error(Self, Error)
        )
    ;   Parent ! failure(Self)
    ).
    
findn0(State, Template, Query, Solutions, Error) :-
    catch(findn(State, Template, Query, Solutions), Error, true).
    
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

pengine_next(Pid) :-
    pengine_next(Pid, []).

pengine_next(Pid, Options) :-
    option(limit(Limit), Options, 1),
    Pid ! pengine:next(Limit).


%!  pengine_stop(+Pid) is det.
%
%   Ask pengine Pid to stop. If successful, delivers a message
%   stop(Pid) to the mailbox of the process that called 
%   pengine_spawn/2-3.

pengine_stop(Pid) :-
    Pid ! pengine:stop. 
    
    
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

pengine_input(Prompt, Input) :-
    self(Self),
    parent(Parent),
    Parent ! prompt(Self, Prompt),
    receive({
        input(Parent, Input) ->
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
%   successful, delivers a message abort(Pid) to the mailbox of the
%   process that called pengine_spawn/2-3.

pengine_abort(Pid) :-
    catch(thread_signal(Pid, throw(exit_query)), _, true).

  


