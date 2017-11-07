:- use_module(node).
:- use_module(node_server).
:- use_module(dispatch).
:- use_module(library(debug)).

%:- debug(ws).
%:- debug(dispatch).

:- op(800, xfx, !).

Pid ! Msg :- send_remote(Pid, Msg).

s(N, Msg) :-
    spawn(start(N, Msg)).

start(NumberProcesses, Message) :-
    get_time(Start),
    self_remote(Self),
    create(NumberProcesses, Self, Message),
    receive({ Msg ->
              get_time(End),
              Wall is End - Start,
              writeln(Msg-Wall) }).

create(1, NextProcess, Message) :- !,
    NextProcess ! Message.
create(NumberProcesses, NextProcess, Message) :-
    Port is (NumberProcesses mod 2) + 3060,
    r(Port, loop(NextProcess), Prev),
    self_remote(Me),
    link(Me, Prev),
    NumberProcesses1 is NumberProcesses - 1,
    create(NumberProcesses1, Prev, Message).

loop(NextProcess) :-
    receive({
        Msg ->
            NextProcess ! Msg
    }).

r(Port, Goal, Id) :-
    format(atom(URL), 'http://localhost:~w/erlang', [Port]),
    spawn_remote(URL, Goal, Id, []).
