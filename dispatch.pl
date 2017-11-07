:- module(dispatch,
          [ start/0,
            flush/0,
            spawn/3,                    % :Goal, -Id, +Options
            send/2,                     % +Id, +Message
            (!)/2,			% +Id, +Message
            receive/1,                  % +Clauses
            link/2,                     % +Parent, +Child
            self/1,                     % -Id

            op(1000, xfx, when),
            op(800, xfx, !)
          ]).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(lists)).

:- meta_predicate
    spawn(0, -, +),
    receive(:).

% :- debug(dispatch).

:- multifile
    hook_self/1,
    hook_spawn/3,
    hook_send/2.


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
    hook_spawn(Goal, Engine, Options),
    !.
spawn(Goal, Engine, Options) :-
    engine_create(true, run(Goal, Options), Engine, Options),
    (   option(link(true), Options)
    ->  self(Me),
        link(Me, Engine)
    ;   true
    ).

self(Pid) :-
    hook_self(Me),
    !,
    Me = Pid.
self(Pid) :-
    engine_self(Me),
    !,
    Me = Pid.
self(thread(Tid)) :-
    thread_self(Tid).

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
    process_get_message(Message, Messages0),
    debug(dispatch, 'Process received ~p', [NewMessage]),
    (   select(Message, [NewMessage|Messages0], Messages1),
        receive_clause(Clauses, Message, Body)
    ->  b_setval(event_queue, Messages1),
        call(M:Body)
    ;   receive(M:{Clauses})
    ).

process_get_message(Message, Messages) :-
    engine_self(_),
    !,
    (   nb_current(event_queue, Messages)
    ->  engine_yield(true)
    ;   Messages = []
    ),
    engine_fetch(Message).
process_get_message(Message, Messages) :-
    (   nb_current(event_queue, Messages)
    ->  true
    ;   Messages = []
    ),
    thread_get_message(!(Message)).

receive_clause((C1;C2), Message, Body) :-
    !,
    (   receive_clause(C1, Message, Body)
    ;   receive_clause(C2, Message, Body)
    ).
receive_clause((HeadAndGuard -> Body), Message, Body) :- !,
    (   subsumes_term(when(Head,Guard), HeadAndGuard)
    ->  when(Head,Guard) = HeadAndGuard,
        subsumes_term(Head, Message),
        Head = Message,
        call(Guard)
    ;   subsumes_term(HeadAndGuard, Message),
        HeadAndGuard = Message
    ),
    debug(dispatch, 'Body: ~p', [Body]).

%!  send(+Pid, +Message) is det.
%!  !(+Pid, +Message) is det.
%
%   Send Message to Pid.

Pid ! Message :-
    send(Pid, Message).

send(Pid, Message) :-
    hook_send(Pid, Message),
    !.
send(thread(Tid), Message) :-
    !,
    thread_send_message(Tid, !(Message)).
send(Pid, Message) :-
    send_local(Pid, user, Message).

send_local(Pid, Type, Message) :-
    start,
    next_dispatch_queue(Queue),
    thread_send_message(Queue, event(Pid, Type, Message)).

destroy_process(Pid) :-
    send_local(Pid, admin, destroy).

%!  flush
%
%   Print all pending messages

flush :-
    thread_self(Me),
    thread_get_message(Me, !(X), [timeout(0)]),
    !,
    print_message(informational, received(X)),
    flush.
flush.

:- multifile
    prolog:message//1.

prolog:message(received(X)) -->
    [ 'Got ~p'-[X] ].
