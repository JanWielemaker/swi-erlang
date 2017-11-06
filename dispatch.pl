:- module(dispatch,
          [ start/0,
            spawn/3,                    % :Goal, -Id, +Options
            send/2,                     % +Id, +Message
            self/1                      % -Id
          ]).
:- use_module(library(debug)).
:- use_module(library(lists)).

:- meta_predicate
    spawn(1, -, +).


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
           (   thread_create(work(Queue), Tid, []),
               assertz(worker(Tid))
           )).

work(Queue) :-
    thread_get_message(Queue, event(Pid, Message)),
    dispatch_event(Pid, Message),
    worker(Queue).

dispatch_event(Pid, Message) :-
    engine_post(Pid, Message, Ok),
    assertion(Ok == true).



		 /*******************************
		 *            PROCESSES		*
		 *******************************/

spawn(Goal, Engine, Options) :-
    engine_create([], process(Goal, []), Engine, Options).

process(Goal, Messages0) :-
    engine_fetch(Message),
    engine_yield(true),
    process_dispatch(Goal, [Message|Messages0], Messages),
    process(Goal, Messages).

process_dispatch(Goal, Messages0, Messages) :-
    select(Message, Messages0, Messages),
    call(Goal, Message),
    !.

self(Pid) :-
    engine_self(Pid).

send(Pid, Message) :-
    next_dispatch_queue(Queue),
    thread_send_message(Queue, event(Pid, Message)).
