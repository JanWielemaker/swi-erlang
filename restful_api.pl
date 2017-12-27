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

:- module(restful_api, []).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(term_to_json)).
:- use_module(library(apply)).
:- use_module(library(time)).

:- use_module(library(debug)).

:- use_module(pengines).
:- use_module(format).


    
:- dynamic
    solution_pengine/3,      % Hash, QueryID, Pid
    solution_index/2.        % Pid, Index
    

         /*******************************
         *          HTTP API            *
         *******************************/


:- http_handler(root(api/pengine_ask), http_pengine_ask, [spawn([])]).

http_pengine_ask(Request) :-
    http_parameters(Request,
        [ query(GoalAtom, []),
          template(TemplateAtom, [default(GoalAtom)]),
          offset(Offset, [integer, default(0)]),
          limit(Limit, [integer, default(1)]),
          timeout(Timeout, [integer, default(1)]),
          format(Format, [default(prolog)])
        ]),
    atomic_list_concat([GoalAtom,+,TemplateAtom], GTAtom),
    read_term_from_atom(GTAtom, Goal+Template, [variable_names(Bindings)]),
    fix_template(Format, Template, Bindings, NewTemplate),
    find_answer(Goal, NewTemplate, Offset, Limit, Timeout, Answer),
    respond(Format, Answer).
    

%!  find_answer(:Query, +Template, +Offset, +Limit, +Timeout, -Answer) is det.

%   - Caching provided by pengines ensures a fast return of
%     consequtive answers.
%
%
%   @tbd: Implement strategies for getting rid of pengines that have
%         been around too long. 
%
%   @tbd: Perhaps the down message would be useful here?

find_answer(Query, Template, Offset, Limit, Timeout, Answer) :-
    query_id(Template-Query, QueryID),
    (   query_pengine(QueryID, Index, Pid),
        Offset == Index
    ->  self(Self),
        pengine_next(Pid, [
            reply_to(Self),
            limit(Limit)
        ]),
        receive({
            success(Pid, Solutions, true) ->
                Answer = success(anonymous, Solutions, true),
                NewIndex is Offset + Limit,
                retractall(solution_index(Pid, _)),
                assertz(solution_index(Pid, NewIndex));
            success(Pid, Solutions, false) ->
                Answer = success(anonymous, Solutions, false),
                cleanup(Pid);
            failure(Pid) ->
                Answer = failure(anonymous),
                cleanup(Pid);
            error(Pid, Error) ->
                Answer = error(anonymous, Error),
                cleanup(Pid);
            after(Timeout) ->
                exit(Pid, timeout),
                Answer = error(anonymous, timeout),
                cleanup(Pid)           
        })
    ;   pengine_spawn(Pid, [
            exit(true)
        ]),
        pengine_ask(Pid, offset(Offset,Query), [ 
            template(Template),
            limit(Limit)
        ]),
        receive({
            success(Pid, Solutions, true) ->
                Answer = success(anonymous, Solutions, true),
                NewIndex is Offset + Limit,
                term_hash(QueryID, Hash),
                assertz(solution_index(Pid, NewIndex)),
                assertz(solution_pengine(Hash, QueryID, Pid));
            success(Pid, Solutions, false) ->
                Answer = success(anonymous, Solutions, false),
                cleanup(Pid);
            failure(Pid) ->
                Answer = failure(anonymous),
                cleanup(Pid);
            error(Pid, Error) ->
                Answer = error(anonymous, Error),
                cleanup(Pid);
            after(Timeout) ->
                exit(Pid, timeout),
                Answer = error(anonymous, timeout),
                cleanup(Pid)
        })      
    ).


query_id(Term, QueryID) :-
    copy_term(Term, QueryID),
    numbervars(QueryID, 0, _).

query_pengine(QueryID, Index, Pengine) :-
    term_hash(QueryID, Hash),
    solution_pengine(Hash, QueryID, Pengine),
    solution_index(Pengine, Index).
    
   
cleanup(Pid) :-
    retractall(solution_pengine(_, _, Pid)),
    retractall(solution_index(Pid, _)).
  

    

