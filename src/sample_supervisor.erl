%% @author Andrew Kreiling <andy@kidzui.com>
%% @copyright 2008 KidZui, Inc. All Rights Reserved.

%% @doc Supervisor for the sample application.

-module(sample_supervisor).
-author('Andrew Kreiling <andy@kidzui.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> Result
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Stomp = {
      stomp_supervisor,
      {stomp_supervisor, start_link, []},
      permanent, infinity, supervisor, [stomp_supervisor]},
    GatewayConfig = [
		     {host, "localhost"},
		     {port, 61613},
		     {login, "login"},
		     {passcode, "passcode"}],
    Gateway = {
      sample_gateway,
      {sample_gateway, start_link, [GatewayConfig]},
      permanent, 5000, worker, [sample_gateway]},
    Processes = [Stomp, Gateway],
    {ok, {{one_for_one, 10, 10}, Processes}}.
