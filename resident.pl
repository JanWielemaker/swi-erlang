:- use_module(actors).

p(X) :- q(X), r(X).

q(a).
q(b).
q(c).
q(d).

r(b).
r(c).
r(d).


server :-
	self(S),
    receive({
        ping(From) ->
            From ! output(S,pong),
            server
    }).