:- use_module(dispatch).

:- op(800, xfx, !).

Pid ! Msg :- send(Pid, Msg).

s(N, Msg) :-
    start,
    spawn(hello, Pid, []),
    Pid ! start(N, Msg).

hello :-
    get_time(Start),
    receive({
        start(N, Msg) -> start(N, Msg),
        hello
        ; hello ->
        get_time(End),
        Time is End - Start,
        writeln(hello(Time))
    }).


start(NumberProcesses, Message) :-
    self(Self),
    create(NumberProcesses, Self, Message).

create(1, NextProcess, Message) :- !,
    NextProcess ! Message.
create(NumberProcesses, NextProcess, Message) :-
    spawn(loop(NextProcess), Prev, [
        %link(true)
    ]),
    self(Me),
    link(Me, Prev),
    NumberProcesses1 is NumberProcesses - 1,
    create(NumberProcesses1, Prev, Message).

loop(NextProcess) :-
    receive({
        Msg ->
            NextProcess ! Msg
    }).
