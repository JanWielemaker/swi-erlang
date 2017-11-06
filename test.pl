:- use_module(dispatch).

pp :-
    spawn(ping, Ping, []),
    spawn(pong(Ping), Pong, []),
    send(Pong, 3).

ping(0-_) :-
    !.
ping(N-Pid) :-
    format('Ping received ~d~n', [N]),
    N2 is N - 1,
    send(Pid, N2).

pong(Ping, N) :-
    format('Pong received ~d~n', [N]),
    self(Pong),
    format('Pong: Sending ~p to ~p~n', [N-Pong, Ping]),
    send(Ping, N-Pong).
