:- module(dispatch,
          [ start/0,
            spawn/3,                    % :Goal, -Id, +Options
            send/2,                     % +Id, +Message
            receive/1,                  % +Clauses
            link/2,                     % +Parent, +Child
            self/1,                     % -Id
            op(1000, xfx, when)
          ]).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(lists)).

:- meta_predicate
    spawn(0, -, +),
    receive(:).

%:- debug(dispatch).

		 /*******************************
		 *             STATE		*
		 *******************************/

:- dynamic
    dispatch_queue/1,
    worker/1,
    linked_child/2.                     % Parent, Child


		 /*******************************
		 *           CONTROL		*
		 *******************************/

start :-
    dispatch_queue(_),
    !.
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
    thread_get_message(Queue, event(Pid, Type, Message)),
    debug(dispatch, 'Got ~p', [event(Pid, Type, Message)]),
    (   dispatch_event(Pid, Type, Message)
    ->  true
    ;   debug(dispatch, 'FAILED ~p', [event(Pid, Type, Message)])
    ),
    work(Queue).

dispatch_event(Pid, admin, destroy) :-
    !,
    engine_destroy(Pid).
dispatch_event(Pid, user, Message) :-
    engine_post(Pid, Message, Ok),
    debug(dispatch, 'Received ~p', [Ok]),
    assertion(Ok == true).



		 /*******************************
		 *            PROCESSES		*
		 *******************************/

%!  spawn(:Goal, -Pid, +Options)
%
%   Spawn a new process.

spawn(Goal, Engine, Options) :-
    engine_create(true, run(Goal, Options), Engine, Options).

self(Pid) :-
    thread_self(Pid).

run(Goal, Options) :-
    setup_call_catcher_cleanup(
        true,
        once(Goal),
        Catcher,
        down(Catcher, Options)).

down(Reason, Options) :-
    option(monitor(Pid), Options),
    self(Me),
    send(Pid, down(Me, Reason)),
    destroy_children(Me).

destroy_children(Me) :-
    forall(retract(linked_child(Me, Child)),
           destroy_process(Child)).

%!  link(+Parent, +Child) is det.
%
%   Define that if Parent goes down, Child is destroyed.

link(Parent, Child) :-
    assertz(linked_child(Parent, Child)).

receive(M:{Clauses}) :-
    (   nb_current(event_queue, Messages0)
    ->  engine_yield(true)
    ;   Messages0 = []
    ),
    engine_fetch(NewMessage),
    debug(dispatch, 'Process received ~p', [NewMessage]),
    (   select(Message, [NewMessage|Messages0], Messages1),
        receive_clause(Clauses, Message, Body)
    ->  b_setval(event_queue, Messages1),
        call(M:Body)
    ;   receive(M:{Clauses})
    ).

receive_clause((C1;C2), Message, Body) :-
    !,
    (   receive_clause(C1, Message, Body)
    ;   receive_clause(C2, Message, Body)
    ).
receive_clause((Head -> when(Guard, Body)), Message, Body) :- !,
    subsumes_term(Head, Message),
    Head = Message,
    once(Guard).
receive_clause((Head->Body), Message, Body) :-
    subsumes_term(Head, Message),
    Head = Message,
    debug(dispatch, 'Body: ~p', [Body]).

send(Pid, Message) :-
    send(Pid, user, Message).

send(Pid, Type, Message) :-
    next_dispatch_queue(Queue),
    thread_send_message(Queue, event(Pid, Type, Message)).

destroy_process(Pid) :-
    send(Pid, admin, destroy).
