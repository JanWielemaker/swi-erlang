/*  Part of SWI-Prolog

    Author:        Torbjörn Lager and Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(distribution,
          [ spawn_remote/4,                     % +Node, :Goal, -Id, +Options
            send_remote/2,                      % +Id, +Message
            self_remote/1,                      % -Id
            register_node_self/1,               % +URL
            op(200, xfx, @)
          ]).
:- use_module(actors).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/websocket)).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(broadcast)).

:- use_module(format).
:- use_module(pengines). % TODO: Eventually remove this

:- dynamic
    websocket/3,                        % Node, Thread, Socket
    self_node/1.                        % Node


:- op(400, fx, debugg).

debugg(Goal) :-
    debug(ws, 'CALL ~p', [Goal]),
    call(Goal),
    debug(ws, 'EXIT ~p', [Goal]).
    

:- http_handler(root(web_prolog), node_manager, [spawn([]), id(web_prolog)]).

node_manager(Request) :-
    http_upgrade_to_websocket(node_loop, [subprotocols([web_prolog])], Request).

node_loop(WebSocket) :-
    ws_receive(WebSocket, Message, [format(json)]),
    (   Message.opcode == close
    ->  retractall(websocket(_,_,WebSocket)),
        thread_self(Me),
        thread_detach(Me)
    ;   Data = Message.data,
        debug(ws, 'Got ~p', [Data]),
        atom_string(Action, Data.action),
        node_action(Action, Data, WebSocket),
        node_loop(WebSocket)
    ).


:- multifile
    hook_node_action/3.

% clauses for bare actors    

node_action(Action, Data, WebSocket) :-
    hook_node_action(Action, Data, WebSocket),
    !.
node_action(spawn, Data, WebSocket) :-
    _{thread:Creator, prolog:String, options:OptionString} :< Data,
    !,
    term_string(Goal, String),
    term_string(Options, OptionString),
    spawn(Goal, Pid, [sandboxed(false)|Options]),
    ws_send(WebSocket, json(_{action:spawned, thread:Creator, pid:Pid})).
node_action(spawned, Data, _WebSocket) :-
    _{thread:CreatorId, pid:Pid} :< Data,
    !,
    thread_property(Creator, id(CreatorId)),
    canonical_pid(Pid, CanPid),
    thread_send_message(Creator, spawned(CanPid)).
node_action(send, Data, _WebSocket) :-
    _{receiver:PidString, prolog:String} :< Data,
    !,
    term_string(Message, String),
    atom_string(Pid, PidString),
    send(Pid, Message).
node_action(send, Data, _WebSocket) :-
    _{thread:Id, prolog:String} :< Data,
    !,
    term_string(Message, String),
    thread_property(Engine, id(Id)),
    send(thread(Engine), Message).
node_action(_Action, Data, _WebSocket) :-
    debug(ws, 'Got unknown data: ~p', [Data]).



:- listen(actor(down, Pid),
          (   debug(ws, 'Removing actor: ~p', [Pid]),
              retractall(current_pengine(Pid, _, _, _))
          )).
          

canonical_pid(Raw, Pid) :-
    (   string(Raw)
    ->  atom_string(Pid, Raw)
    ;   Pid = Raw
    ).

%!  connection(+Node, -Socket)
%
%   Return an existing connection or create a connection to Node.

connection(Node, Socket) :-
    websocket(Node, _Thread, Socket),
    !.
connection(Node, Socket) :-
    http_open_websocket(Node, Socket, [subprotocol(web_prolog)]),
    thread_create(node_loop(Socket), Thread, [alias(Node)]),
    assertz(websocket(Node, Thread, Socket)).

%!  spawn_remote(+Node, :Goal, -Id, +Options)
%
%   Spawn a process on a remote node.

spawn_remote(Node, Goal, Id@Node, Options) :-
    connection(Node, Socket),
    term_string(Goal, String),
    term_string(Options, OptionString),
    thread_self(Me),
    thread_property(Me, id(MyId)),
    ws_send(Socket, json(_{action:spawn, thread:MyId, prolog:String, options:OptionString})),
    thread_get_message(Me, spawned(Id)).

%!  send_remote(Id, Message) :-
%
%   Send a message to a remote process.
%
%   @tbd: 


:- multifile
    hook_send_remote/2.

send_remote(Pid, Message) :-
    hook_send_remote(Pid, Message),
    !.
send_remote(thread(Id)@Node, Message) :-
    !,
    connection(Node, Socket),
    term_string(Message, String),
    ws_send(Socket, json(_{action:send, thread:Id, prolog:String})).
send_remote(Id@Node, Message) :-
    connection(Node, Socket),
    term_string(Message, String),
    ws_send(Socket, json(_{action:send, receiver:Id, prolog:String})).

%!  self_remote(-Self)
%
%   Get a global identifier for self.

self_remote(Id@Node) :-
    engine_self(Id),
    !,
    self_node(Node).
self_remote(thread(Id)@Node) :-
    thread_self(Thread),
    !,
    self_node(Node),
    thread_property(Thread, id(Id)).

%!  register_node_self(+URL)
%
%   Register the name by which this node is known

register_node_self(URL) :-
    asserta(self_node(URL)).


		 /*******************************
		 *    EXTEND LOCAL PROCESSES	*
		 *******************************/

:- multifile
    actors:hook_self/1.

actors:hook_self(Me) :-
    self_remote(Me).

actors:hook_spawn(Goal, Engine, Options) :-
    select_option(node(Node), Options, RestOptions),
    !,
    spawn_remote(Node, Goal, Engine, RestOptions).

actors:hook_send(Id@Node, Message) :-
    !,
    (   self_node(Node)
    ->  (   Id = thread(Tid)
        ->  thread_property(Thread, id(Tid)),
            send(thread(Thread), Message)
        ;   %thread_property(Engine, id(Id)),
            send(Id, Message)
        )
    ;   send_remote(Id@Node, Message)
    ).
actors:hook_send(Socket, Message) :-
    send_remote(Socket, Message).

