%% @author Andrew Kreiling <andy@kidzui.com>
%% @copyright 2008 KidZui, Inc. All Rights Reserved.

%% @doc Supervisor for the stomp clients.

-module(stomp_supervisor).
-author('Andrew Kreiling <andy@kidzui.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> Result
%% @doc API for stating the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    {ok, {{simple_one_for_one, 0, 1},
	  [{stomp_client,
	    {stomp_client, start_link, []},
	    temporary, brutal_kill, worker, [stomp_client]}
	  ]}}.
