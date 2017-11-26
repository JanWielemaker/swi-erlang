/* In Erlang:

See: http://erlang.org/doc/getting_started/conc_prog.html

ping(0, Pong_PID) ->
    Pong_PID ! finished,
    io:format("ping finished~n", []);

ping(N, Pong_PID) ->
    Pong_PID ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n", [])
    end,
    ping(N - 1, Pong_PID).

pong() ->
    receive
        finished ->
            io:format("Pong finished~n", []);
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong()
    end.

start() ->
    Pong_PID = spawn(tut15, pong, []),
    spawn(tut15, ping, [3, Pong_PID]).

*/


ping(0, Pong_Pid) :-
    Pong_Pid ! finished,
    writeln('Ping finished').
ping(N, Pong_Pid) :-
    self(Self),
    Pong_Pid ! ping(Self),
    receive({
        pong ->
            writeln('Ping received pong')
    }),
    N1 is N - 1,
    ping(N1, Pong_Pid).
    
pong :-
    receive({
        ping(Ping_Pid) ->
            writeln('Pong received ping'),
            Ping_Pid ! pong,
            pong;
        finished ->
            writeln('Pong finished')
    }).
   
start :-
    spawn(pong, Pong_Pid),
    spawn(ping(3, Pong_Pid), _).
    