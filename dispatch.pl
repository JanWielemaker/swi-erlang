:- module(dispatch,
          [ start/0,
            spawn/3,                    % :Goal, -Id, +Options
            send/2,                     % +Id, +Message
            receive/1,                  % +Clauses
            self/1                      % -Id
          ]).
:- use_module(library(debug)).
:- use_module(library(lists)).

:- meta_predicate
    spawn(0, -, +),
    receive(:).

:- debug(dispatch).

		 /*******************************
		 *             STATE		*
		 *******************************/

:- dynamic
    dispatch_queue/1,
    worker/1.


		 /*******************************
		 *           CONTROL		*
		 *******************************/

start :-
    make_dispatch_queues(1),
    make_workers(1).


make_dispatch_queues(N) :-
    forall(between(1, N, _),
           (   message_queue_create(Queue, []),
               assertz(dispatch_queue(Queue))
           )).

next_dispatch_queue(Q) :-
    dispatch_queue(Q).

make_workers(N) :-
    dispatch_queue(Queue),
    forall(between(1, N, _),
           (   thread_create(work(Queue), Tid, [alias(dispatch)]),
               assertz(worker(Tid))
           )).

work(Queue) :-
    thread_get_message(Queue, event(Pid, Message)),
    debug(dispatch, 'Got ~p', [event(Pid, Message)]),
    dispatch_event(Pid, Message),
    work(Queue).

dispatch_event(Pid, Message) :-
    engine_post(Pid, Message, Ok),
    debug(dispatch, 'Received ~p', [Ok]),
    assertion(Ok == true).



		 /*******************************
		 *            PROCESSES		*
		 *******************************/

spawn(Goal, Engine, Options) :-
    engine_create(true, run(Goal), Engine, Options).

self(Pid) :-
    engine_self(Pid).

run(Goal) :-
    call(Goal).

receive(M:{Clauses}) :-
    (   nb_current(event_queue, Messages0)
    ->  engine_yield(true)
    ;   Messages0 = []
    ),
    engine_fetch(NewMessage),
    debug(dispatch, 'Process received ~p', [NewMessage]),
    select(Message, [NewMessage|Messages0], Messages1),
    receive_clause(Clauses, Message, Body),
    !,
    b_setval(event_queue, Messages1),
    call(M:Body).

receive_clause((C1;C2), Message, Body) :-
    !,
    (   receive_clause(C1, Message, Body)
    ;   receive_clause(C2, Message, Body)
    ).
receive_clause((Head->Body), Head, Body) :-
    debug(dispatch, 'Body: ~p', [Body]).

send(Pid, Message) :-
    next_dispatch_queue(Queue),
    thread_send_message(Queue, event(Pid, Message)).

