%% @author Andrew Kreiling <andy@kidzui.com>
%% @copyright 2008 KidZui, Inc. All Rights Reserved.

%% @doc Callbacks for the sample application.

-module(sample_app).
-author('Andrew Kreiling <andy@kidzui.com>').

-behaviour(application).

%% External exports
-export([start/2, stop/1]).

%% @spec start(_Type, _StartArgs) -> Result
%% @doc application start callback for sample.
start(_Type, _StartArgs) ->
    sample_supervisor:start_link().

%% @spec stop(_State) -> ok
%% @doc application stop callback for sample.
stop(_State) ->
    ok.
