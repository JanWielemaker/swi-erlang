:- use_module(erlang).
:- use_module(library(debug)).

%:- debug(ws).
%:- debug(dispatch).

/** <module> Test handling source

```
$ swipl -g 'node_server(localhost:3061)' remote.pl
$ swipl -g 'node_server(localhost:3060)' remote.pl

?- test([node('http://localhost:3061/erlang')]).
```

*/

test(Options) :-
    spawn(hello, _Id,
          [ src_text("hello :- writeln(hello), sleep(1), hello.")
          | Options
          ]).

