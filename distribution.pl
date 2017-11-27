/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
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
            register_node_self/1                % +URL
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
    self_node/1,                        % Node
    actor_uuid/2,                       % Engine, ID
    creator_ws/3.                       % Creator, Socket


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

% clauses for the pengine protocol

node_action(pengine_spawn, Data, WebSocket) :-
    _{options:OptionString} :< Data,
    !,
    term_string(Options, OptionString),
    option(reply_to(ReplyTo), Options),
    select_option(format(Format), Options, RestOptions, 'json-s'),
    pengine_spawn(Engine, [sandboxed(false)|RestOptions]),
    actor_uuid(UUID),
    asserta(actor_uuid(Engine, UUID)),
    asserta(creator_ws(ReplyTo, WebSocket, Format)),    
    ws_send(WebSocket, json(_{type:spawned, pid:UUID})).
node_action(pengine_ask, Data, WebSocket) :-
    _{pid:UUIDString, goal:String, options:OptionString} :< Data,
    !,
    read_term_from_atom(String, Goal, [variable_names(Bindings)]),    
    term_string(Options, OptionString),
    atom_string(UUID, UUIDString),
    actor_uuid(Engine, UUID),
    creator_ws(_Creator, WebSocket, Format),
    fix_template(Format, Goal, Bindings, NewTemplate),
    pengine_ask(Engine, Goal, [template(NewTemplate)|Options]).
node_action(pengine_next, Data, _WebSocket) :-
    _{pid:UUIDString, options:OptionString} :< Data,
    !,
    term_string(Options, OptionString),
    atom_string(UUID, UUIDString),
    actor_uuid(Engine, UUID),
    pengine_next(Engine, Options).    
node_action(pengine_stop, Data, _WebSocket) :-
    _{pid:UUIDString, options:OptionString} :< Data,
    !,
    term_string(Options, OptionString),
    atom_string(UUID, UUIDString),
    actor_uuid(Engine, UUID),
    pengine_stop(Engine, Options).
node_action(pengine_respond, Data, _WebSocket) :-
    _{pid:UUIDString, prolog:String} :< Data,
    !,
    term_string(Term, String),
    atom_string(UUID, UUIDString),
    actor_uuid(Engine, UUID),
    pengine_respond(Engine, Term).
node_action(pengine_abort, Data, _WebSocket) :-
    _{pid:UUIDString} :< Data,
    !,
    atom_string(UUID, UUIDString),
    actor_uuid(Engine, UUID),
    pengine_abort(Engine).

% clauses for bare actors    

node_action(spawn, Data, WebSocket) :-
    _{thread:Creator, prolog:String, options:OptionString} :< Data,
    !,
    term_string(Goal, String),
    term_string(Options, OptionString),
    spawn(Goal, Engine, [sandboxed(false)|Options]),
    actor_uuid(UUID),
    asserta(actor_uuid(Engine, UUID)),
    ws_send(WebSocket, json(_{action:spawned, thread:Creator, pid:UUID})).
node_action(spawned, Data, _WebSocket) :-
    _{thread:CreatorId, pid:Id} :< Data,
    !,
    thread_property(Creator, id(CreatorId)),
    canonical_id(Id, CannId),
    thread_send_message(Creator, spawned(CannId)).
node_action(send, Data, _WebSocket) :-
    _{receiver:UUIDString, prolog:String} :< Data,
    !,
    term_string(Message, String),
    atom_string(UUID, UUIDString),
    actor_uuid(Engine, UUID),
    send(Engine, Message).
node_action(send, Data, _WebSocket) :-
    _{thread:Id, prolog:String} :< Data,
    !,
    term_string(Message, String),
    thread_property(Engine, id(Id)),
    send(thread(Engine), Message).
node_action(_Action, Data, _WebSocket) :-
    debug(ws, 'Got unknown data: ~p', [Data]).



:- listen(actor(down, Engine),
          retractall(actor_uuid(Engine, _))).

actor_uuid(Module) :-
        uuid(Module, [version(4)]).

canonical_id(Raw, ID) :-
    (   string(Raw)
    ->  atom_string(ID, Raw)
    ;   ID = Raw
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

spawn_remote(Node, Goal, process(Node,Id), Options) :-
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

send_remote(Creator, Message) :-
    creator_ws(Creator, Socket, Format),
    !,
    %term_string(Message, String),
    answer_format(Message, Json, Format),
    ws_send(Socket, json(Json)).
send_remote(process(Node,thread(Id)), Message) :-
    !,
    connection(Node, Socket),
    term_string(Message, String),
    ws_send(Socket, json(_{action:send, thread:Id, prolog:String})).
send_remote(process(Node,Id), Message) :-
    connection(Node, Socket),
    term_string(Message, String),
    ws_send(Socket, json(_{action:send, receiver:Id, prolog:String})).

%!  self_remote(-Self)
%
%   Get a global identifier for self.

self_remote(process(Node, Id)) :-
    engine_self(Engine),
    !,
    self_node(Node),
    thread_property(Engine, id(Id)).
self_remote(process(Node, thread(Id))) :-
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

actors:hook_send(process(Node, Id), Message) :-
    !,
    (   self_node(Node)
    ->  (   Id = thread(Tid)
        ->  thread_property(Thread, id(Tid)),
            send(thread(Thread), Message)
        ;   thread_property(Engine, id(Id)),
            send(Engine, Message)
        )
    ;   send_remote(process(Node, Id), Message)
    ).
actors:hook_send(Socket, Message) :-
    send_remote(Socket, Message).

