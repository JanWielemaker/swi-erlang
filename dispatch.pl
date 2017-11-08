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

            dump_backtrace/2,           % +Id, +Depth
            dump_queue/2,               % +Id, -Queue

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
    make_workers(5).


make_dispatch_queues(N) :-
    forall(between(1, N, _),
           (   message_queue_create(Queue, []),
               assertz(dispatch_queue(Queue))
           )).

next_dispatch_queue(Q) :-
    dispatch_queue(Q).

make_workers(N) :-
    dispatch_queue(Queue),
    forall(between(1, N, I),
           (   atom_concat(dispatch_, I, Alias),
               thread_create(work(Queue), Tid, [alias(Alias)]),
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
    catch(post_true(Pid, Message), E,
          (   format('Failed to deliver ~p to ~p~n', [Message, Pid]),
              print_message(error, E))).

post_true(Pid, Message) :-
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
    self(Self),
    debug(dispatch(down), '~p down on ~p', [Self, Reason]),
    (   option(monitor(Pid), Options)
    ->  send(Pid, down(Self, Reason))
    ;   true
    ),
    retractall(registered(_, Self)),
    destroy_children(Self).

destroy_children(Me) :-
    forall(retract(linked_child(Me, Child)),
           destroy_process(Child)).

%!  link(+Parent, +Child) is det.
%
%   Define that if Parent goes down, Child is destroyed.

link(Parent, Child) :-
    assertz(linked_child(Parent, Child)).

receive(Clauses) :-
    process_queue(Queue0),
    self(Self),
    debug(dispatch(receive), '~p queue: ~p', [Self, Queue0]),
    (   select(Message, Queue0, Queue1),
        receive_clause(Clauses, Message, Body)
    ->  b_setval(event_queue, Queue1),
        call_body(Clauses, Body)
    ;   process_get_message(New),
        b_setval(event_queue, [New|Queue0]),
        receive(Clauses)
    ).

call_body(M:_, Body) :-
    debug(dispatch(call), 'Calling ~p', [M:Body]),
    (   call(M:Body)
    *-> true
    ;   format('Body failed: ~p~n', [Body])
    ).

process_queue(Queue) :-
    nb_current(event_queue, Queue),
    !.
process_queue([]).

process_get_message(Message) :-
    engine_self(_),
    !,
    (   nb_current(event_queue, _)
    ->  engine_yield(true)
    ;   b_setval(event_queue, [])
    ),
    engine_fetch(Message0),
    service_message(Message0, Message).
process_get_message(Message) :-
    thread_get_message(!(Message)).

service_message(Message0, Message) :-
    nonvar(Message0),
    service_message2(Message0, Reply), !,
    engine_yield(Reply),
    engine_fetch(Message1),
    service_message(Message1, Message).
service_message(Message, Message).

service_message2('$start', true).
service_message2('$backtrace'(Depth), true) :-
    set_prolog_flag(backtrace_goal_depth, 10),
    backtrace(Depth).
service_message2('$queue', Queue) :-
    process_queue(Queue).


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

		 /*******************************
		 *             DEBUG		*
		 *******************************/

dump_backtrace(Id, Depth) :-
    thread_property(E, id(Id)), !,
    engine_post(E, '$backtrace'(Depth), _).

dump_queue(Id, Queue) :-
    thread_property(E, id(Id)), !,
    engine_post(E, '$queue', Queue).

user:portray(Engine) :-
    is_engine(Engine),
    registered(Alias, Engine),
    writeq(Alias).

		 /*******************************
		 *            MESSAGES		*
		 *******************************/

:- multifile
    prolog:message//1.

prolog:message(received(X)) -->
    [ 'Got ~p'-[X] ].
