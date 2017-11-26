/* In Erlang:

See: http://learnyousomeerlang.com/more-on-multiprocessing


fridge(FoodList) ->
    receive
        {From, {store, Food}} ->
            From ! {self(), ok},
            fridge([Food|FoodList]);
        {From, {take, Food}} ->
            case lists:member(Food, FoodList) of
                true ->
                    From ! {self(), {ok, Food}},
                    fridge(lists:delete(Food, FoodList));
                false ->
                    From ! {self(), not_found},
                    fridge(FoodList)
            end;
        terminate ->
            ok
    end.

*/


start(Pid) :-
    spawn(fridge([]), Pid).
    

fridge(FoodList0) :-
    receive({
        store(From, Food) ->
            From ! ok,
            fridge([Food|FoodList0]);
        take(From, Food) ->
            (   select(Food, FoodList0, FoodList)
            ->  From ! ok(Food),
                fridge(FoodList)
            ;   From ! not_found,
                fridge(FoodList0)
            );
        terminate ->
            true
    }).


/** Examples

?- start(Pid).
    
?- self(Self).
    
?- $Pid ! store($Self, meat).
    
?- flush.
    
?- $Pid ! take($Self, meat).
    
?- flush.

*/
