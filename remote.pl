:- use_module(node).
:- use_module(node_server).
:- use_module(dispatch).
:- use_module(library(debug)).
:- use_module(srctext).

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
    spawn(with_source(hello,
                      [ src_text("hello :- writeln(hello), sleep(1), hello.")
                      ]),
          _Id,
          Options).

