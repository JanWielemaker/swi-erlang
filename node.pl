:- module(node,
          [ spawn_remote/4,                     % +Node, :Goal, -Id, +Options
            send_remote/2,                      % +Id, +Message
            self_remote/1                       % -Id
          ]).
:- use_module(dispatch).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/websocket)).
:- use_module(library(http/http_host)).
:- use_module(library(debug)).

:- dynamic
    websocket/3,                           % Node, Thread, Socket
    self_node/1.                           % Node

:- http_handler(root(erlang), node_manager, [spawn([])]).

node_manager(Request) :-
    http_upgrade_to_websocket(node_loop, [], Request).

node_loop(WebSocket) :-
    ws_receive(WebSocket, Message, [format(json)]),
    (   Message.opcode == close
    ->  true
    ;   Data = Message.data,
        debug(ws, 'Got ~p', [Data]),
        atom_string(Action, Data.action),
        node_action(Action, Data, WebSocket),
        node_loop(WebSocket)
    ).

node_action(spawn, Data, WebSocket) :-
    _{thread:Creator, prolog:String} :< Data,
    !,
    term_string(Goal, String),
    spawn(Goal, Engine, []),
    thread_property(Engine, id(Id)),
    ws_send(WebSocket, json(_{action:spawned, thread:Creator, pid:Id})).
node_action(spawned, Data, _WebSocket) :-
    _{thread:CreatorId, pid:Id} :< Data,
    !,
    thread_property(Creator, id(CreatorId)),
    thread_send_message(Creator, spawned(Id)).
node_action(send, Data, _WebSocket) :-
    _{receiver:Id, prolog:String} :< Data,
    !,
    term_string(Message, String),
    thread_property(Engine, id(Id)),
    send_local(Engine, Message).
node_action(_Action, Data, _WebSocket) :-
    debug(ws, 'Got unknown data: ~p', [Data]).

%!  connection(+Node, -Socket)
%
%   Return an existing connection or create a connection to Node.

connection(Node, Socket) :-
    websocket(Node, _Thread, Socket),
    !.
connection(Node, Socket) :-
    http_open_websocket(Node, Socket, [subprotocol(erlang)]),
    thread_create(node_loop(Socket), Thread, [alias(Node)]),
    assertz(websocket(Node, Thread, Socket)).

%!  spawn_remote(+Node, :Goal, -Id, +Options)
%
%   Spawn a process on a remote node.

spawn_remote(Node, Goal, process(Node,Id), _Options) :-
    connection(Node, Socket),
    term_string(Goal, String),
    thread_self(Me),
    thread_property(Me, id(MyId)),
    ws_send(Socket, json(_{action:spawn, thread:MyId, prolog:String})),
    thread_get_message(Me, spawned(Id)).

%!  send_remote(Id, Message) :-
%
%   Send a message to a remote process.

send_remote(process(Node,Id), Message) :-
    connection(Node, Socket),
    term_string(Message, String),
    ws_send(Socket, json(_{action:send, receiver:Id, prolog:String})).

%!  self_remote(-Self)
%
%   Get a global identifier for self.

self_remote(process(Node, Id)) :-
    self_node(Node),
    self(Engine),
    thread_property(Engine, id(Id)).
