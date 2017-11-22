:- op(800, xfx, !).
:- op(1000, xfy, when).

Pid ! Message :-
    send(Pid, Message).
    
    
important(Messages) :-
    receive({
        Priority-Message when Priority > 10 ->
            Messages = [Message|MoreMessages],
            important(MoreMessages)
    }, [ timeout(0),
         on_timeout(normal(Messages))
    ]).

normal(Messages) :-
    receive({
        _-Message ->
            Messages = [Message|MoreMessages],
            normal(MoreMessages)
    }, [ timeout(0),
         on_timeout(Messages=[])
    ]).
        

/** Examples

self(S), send(S, 15-high), send(S, 7-low), send(S, 1-low), send(S, 17-high)

*/

