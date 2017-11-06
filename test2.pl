:- use_module(dispatch).

pp :-
    spawn(ping, Ping, [alias(ping)]),
    spawn(pong(Ping), Pong, [alias(pong)]),
    send(Pong, 3).

ping :-
    receive({   0-_ -> true
            ;   N-Pong ->
                format('Ping received ~d~n', [N]),
                N2 is N - 1,
                send(Pong, N2),
                ping
            }).

pong(Ping) :-
    receive({ N ->
              format('Pong received ~d~n', [N]),
              self(Pong),
              format('Pong: Sending ~p to ~p~n', [N-Pong, Ping]),
              send(Ping, N-Pong),
              pong(Ping)
            }).
