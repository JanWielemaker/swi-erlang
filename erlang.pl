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
