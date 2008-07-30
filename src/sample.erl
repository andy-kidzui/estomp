%% @author Andrew Kreiling <andy@kidzui.com>
%% @copyright 2008 KidZui, Inc. All Rights Reserved.

%% @doc Sample application for estomp

-module(sample).
-author('Andrew Kreiling <andy@kidzui.com>').

%% External exports
-export([start/0, stop/0]).

%% @spec start() -> ok
%% @doc Start the sample server.
start() ->
    application:start(sample).

%% @spec stop() -> ok
%% @doc Stop the sample server.
stop() ->
    application:stop(sample).
