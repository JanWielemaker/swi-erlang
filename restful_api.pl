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

:- use_module(library(debug)).

:- use_module(pengines).



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
          format(Format, [default(prolog)])
        ]),
    atomic_list_concat([GoalAtom,+,TemplateAtom], GTAtom),
    read_term_from_atom(GTAtom, Goal+Template, [variable_names(Bindings)]),
    fix_template(Format, Template, Goal, Bindings, NewTemplate),
    find_answer(Goal, NewTemplate, Offset, Limit, Answer),
    respond(Format, Answer).


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
    
        
    
:- dynamic
    solution_pengine/4,      % Hash, QueryID, PengineID, Queue
    solution_index/2.        % PengineID, Index


%!  find_answer(:Query, +Template, +Offset, +Limit, -Answer) is det.
%
%   @tbd: Implement strategies for getting rid of pengines that have
%         been around too long. 

find_answer(Query, Template, Offset, Limit, Answer) :-
    query_id(Template-Query, QueryID),
    (   query_pengine(QueryID, Index, Pid, Queue),
        Offset == Index
    ->  pengine_next(Pid, [
            limit(Limit)
        ]),
        retractall(solution_index(Pid, _))
    ;   message_queue_create(Queue),
        pengine_spawn(Pid, [
            reply_to(thread(Queue)),
            exit(true)
        ]),
        pengine_ask(Pid, Query, [ 
            template(Template),
            offset(Offset),
            limit(Limit)
        ]),
        term_hash(QueryID, Hash),
        assertz(solution_pengine(Hash, QueryID, Pid, Queue))
    ),
    NewIndex is Offset + Limit,
    assertz(solution_index(Pid, NewIndex)),
    thread_get_message(Queue, !(Answer0)),
    process_answer(Answer0, Answer).



query_id(Term, QueryID) :-
    copy_term(Term, QueryID),
    numbervars(QueryID, 0, _).

query_pengine(QueryID, Index, Pengine, Queue) :-
    term_hash(QueryID, Hash),
    solution_pengine(Hash, QueryID, Pengine, Queue),
    solution_index(Pengine, Index).
    


process_answer(success(Pid, Solutions, More),
               success(anonymous, Solutions, More)) :-
    (   More == false
    ->  cleanup(Pid)
    ;   true
    ).
process_answer(failure(Pid), failure(anonymous)) :-
    cleanup(Pid).
process_answer(error(Pid, Error), error(anonymous, Error)) :-
    cleanup(Pid).
    
    
cleanup(Pid) :-
    retractall(solution_pengine(_, _, Pid, _)),
    retractall(solution_index(Pid, _)).
    
    
    
         /*******************************
         *     RESPONSE GENERATION      *
         *******************************/
         

respond(prolog, Answer) :- !,
    format('Content-type: text/plain;~n~n'),
    format("~q.", [Answer]).
respond(Format, Answer) :-
    json_lang(Format), !,    
    answer_format(Answer, JSON, Format),
    reply_json(JSON).
    

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
	JSON = json{type:success, pid:ID, data:Answers, more:More},
	maplist(answer_to_json_strings, Answers0, Answers).              
answer_format(failure(Pid),
              json{type:failure, pid:Pid},
              'json-s') :- !.
answer_format(stop(Pid),
              json{type:stop, pid:Pid},
              'json-s') :- !.
answer_format(error(Pid, ErrorTerm),
              json{type:error, pid:Pid, data:Message},
              'json-s') :- !,
    message_to_string(ErrorTerm, Message).
answer_format(prompt(Pid, Term),
              json{type:prompt, pid:Pid, data:Term},
              'json-s') :- !.
answer_format(output(Pid, Term),
              json{type:output, pid:Pid, data:JSON},
              'json-s') :- !,
    map_output(Term, JSON).
answer_format(down(Pid, ErrorTerm),
              json{type:down, pid:Pid, data:Message},
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
    
%%	answer_to_json_strings(+AnswerDictIn, -AnswerDict).
%
%	Translate answer dict with Prolog term   values into answer dict
%	with string values.

answer_to_json_strings(DictIn, DictOut) :-
	dict_pairs(DictIn, Tag, Pairs),
	maplist(term_string_value, Pairs, BindingsOut),
	dict_pairs(DictOut, Tag, BindingsOut).

term_string_value(N-V, N-A) :-
	with_output_to(string(A),write_term(V,
				  [ quoted(true)
				  ])).


