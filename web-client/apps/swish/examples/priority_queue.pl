
important(Messages) :-
    receive({
        Priority-Message when Priority > 10 ->
            Messages = [Message|MoreMessages],
            important(MoreMessages);
    	after(0) -> 
            normal(Messages)
    }).

normal(Messages) :-
    receive({
        _-Message ->
            Messages = [Message|MoreMessages],
            normal(MoreMessages);
    	after(0) -> 
            Messages = []
    }).
        

/** Examples

self(S), S ! 15-high, S ! 7-low, S ! 1-low, S ! 17-high.
    
important(Messages).

*/