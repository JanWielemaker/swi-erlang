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

:- module(erlang,
          [ flush/0,                    % From dispatch
            spawn/1,                    % :Goal
            spawn/2,                    % :Goal, -Id
            spawn/3,                    % :Goal, -Id, +Options
            send/2,                     % +Id, +Message
            (!)/2,			% +Id, +Message
            exit/1,                     % +Reason
            exit/2,                     % +Id, +Reason
            receive/1,                  % +Clauses
            link/2,                     % +Parent, +Child
            self/1,                     % -Id
            register/2,                 % +Alias, +Pid
            unregister/1,		% +Alias

            dump_backtrace/2,           % +Id, +Depth
            dump_queue/2,               % +Id, -Queue

            op(1000, xfx, when),
            op(800, xfx, !),

            node_server/0,              % from node_server
            node_server/1
          ]).
:- use_module(dispatch).
:- use_module(node).
:- use_module(node_server).
:- use_module(srctext).
:- use_module(library(option)).

:- multifile
    dispatch:hook_goal/3.

dispatch:hook_goal(Goal0, srctext:with_source(Goal0, GoalOptions), Options) :-
    \+ option(node(_), Options),
    actor_uuid(Module),
    GoalOptions = [ module(Module)
                  | Options
                  ].

actor_uuid(Module) :-
        uuid(Module, [version(4)]).
