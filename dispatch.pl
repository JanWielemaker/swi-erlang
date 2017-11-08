:- module(dispatch,
          [ start/0,
            flush/0,
            spawn/1,                    % :Goal
            spawn/2,                    % :Goal, -Id
            spawn/3,                    % :Goal, -Id, +Options
            send/2,                     % +Id, +Message
            (!)/2,			% +Id, +Message
            receive/1,                  % +Clauses
            link/2,                     % +Parent, +Child
            self/1,                     % -Id
            register/2,                 % +Alias, +Pid
            unregister/1,		% +Alias

            op(1000, xfx, when),
            op(800, xfx, !)
          ]).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(lists)).

:- meta_predicate
    spawn(0),
    spawn(0, -),
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
    registered/2,                       % Name, Id
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
    debug(dispatch(queue), 'Got ~p', [event(Pid, Type, Message)]),
    (   dispatch_event(Pid, Type, Message)
    ->  true
    ;   debug(dispatch(dispatch), 'FAILED ~p', [event(Pid, Type, Message)])
    ),
    work(Queue).

dispatch_event(Pid, admin, destroy) :-
    !,
    engine_destroy(Pid).
dispatch_event(Pid, user, Message) :-
    debug(dispatch(wakeup), 'Wakeup ~p for ~p', [Pid, Message]),
    engine_post(Pid, Message, Ok),
    debug(dispatch(wakeup), 'Wakeup ~p replied ~p', [Pid, Ok]),
    assertion(Ok == true).


		 /*******************************
		 *            PROCESSES		*
		 *******************************/

%!  spawn(:Goal) is det.
%!  spawn(:Goal, -Pid) is det.
%!  spawn(:Goal, -Pid, +Options) is det.
%
%   Spawn a new process.

spawn(Goal) :-
    spawn(Goal, _, []).

spawn(Goal, Engine) :-
    spawn(Goal, Engine, []).

spawn(Goal, Engine, Options) :-
    select_option(monitor(true), Options, Options1),
    !,
    self(Me),
    spawn2(Goal, Engine, [monitor(Me)|Options1]).
spawn(Goal, Engine, Options) :-
    spawn2(Goal, Engine, Options).

spawn2(Goal, Engine, Options) :-
    hook_spawn(Goal, Engine, Options),
    !.
spawn2(Goal, Engine, Options) :-
    engine_create(true, run(Goal, Options), Engine, Options),
    (   option(link(true), Options)
    ->  self(Me),
        link(Me, Engine)
    ;   true
    ),
    send(Engine, '$start').

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

receive(Clauses) :-
    process_get_queue(Queue0),
    self(Me),
    debug(dispatch(receive), '~p queue: ~p', [Me, Queue0]),
    dispatch(Queue0, Clauses, Queue1),
    (   Queue0 == Queue1
    ->  receive(Clauses)
    ;   b_setval(event_queue, Queue1)
    ).

dispatch(Queue0, Clauses, Queue) :-
    select(Message, Queue0, Queue1),
    receive_clause(Clauses, Message, Body),
    !,
    Clauses = M:_,
    debug(dispatch(call), 'Calling ~p', [M:Body]),
    call_body(M:Body),
    dispatch(Queue1, Clauses, Queue).
dispatch(Queue, _, Queue).

call_body(Body) :-
    (   call(Body)
    *-> true
    ;   format('Body failed: ~p~n', [Body])
    ).

process_get_queue([Message|Queue]) :-
    engine_self(_),
    !,
    (   nb_current(event_queue, Queue)
    ->  engine_yield(true)
    ;   Queue = [],
        b_setval(event_queue, [])
    ),
    engine_fetch(Message0),
    (   Message0 == '$start'
    ->  engine_yield(true),
        engine_fetch(Message)
    ;   Message = Message0
    ).
process_get_queue([Message|Queue]) :-
    (   nb_current(event_queue, Queue)
    ->  true
    ;   Queue = []
    ),
    thread_get_message(!(Message)).

receive_clause(_M:{Clauses}, Message, Body) :-
    receive_clause2(Clauses, Message, Body).

receive_clause2((C1;C2), Message, Body) :-
    !,
    (   receive_clause2(C1, Message, Body)
    ;   receive_clause2(C2, Message, Body)
    ).
receive_clause2((HeadAndGuard -> Body), Message, Body) :- !,
    (   subsumes_term(when(Head,Guard), HeadAndGuard)
    ->  when(Head,Guard) = HeadAndGuard,
        subsumes_term(Head, Message),
        Head = Message,
        call(Guard)
    ;   subsumes_term(HeadAndGuard, Message),
        HeadAndGuard = Message
    ),
    debug(dispatch(match), 'Message: ~p, body: ~p', [Message, Body]).

%!  send(+Pid, +Message) is det.
%!  !(+Pid, +Message) is det.
%
%   Send Message to Pid.

Pid ! Message :-
    send(Pid, Message).

send(Alias, Message) :-
    registered(Alias, Pid),
    !,
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
    debug(dispatch(send), 'Sending ~p ! ~p', [Pid, Message]),
    thread_send_message(Queue, event(Pid, Type, Message)).

destroy_process(Pid) :-
    send_local(Pid, admin, destroy).

%!  register(+Alias, +Pid) is det.
%!  unregister(+Alias) is det.

register(Alias, Pid) :-
    asserta(registered(Alias, Pid)).

unregister(Alias) :-
    retractall(registered(Alias, _)).

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

user:portray(Engine) :-
    is_engine(Engine),
    registered(Alias, Engine),
    writeq(Alias).
