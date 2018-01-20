/*
    This is the node-resident code available in this node. It is loaded
    in the module `web_prolog' using the following code:

    :- user:consult(resident).
*/

:- use_module(web_prolog).

p(X) :- q(X), r(X).

q(a).
q(b).
q(c).
q(d).

r(b).
r(c).
r(d).


% Node-resident pingpong server:

server :-
    receive({
        ping(From) ->
            From ! pong,
            server
    }).
    
:- spawn(server, Pid),
   register(pingpong, Pid).