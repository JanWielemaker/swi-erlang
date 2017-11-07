:- module(node_server,
          [ node_server/0,
            node_server/1
          ]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

node_server :-
    node_server(localhost:3060).

node_server(localhost:Port) :-
    format(atom(URL), 'http://localhost:~w/erlang', [Port]),
    assertz(node:self_node(URL)),
    http_server(http_dispatch,
                [ port(localhost:Port)
                ]).
