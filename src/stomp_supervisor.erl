-module(stomp_supervisor).
-author('Andrew Kreiling <andy@kidzui.com>').
-behaviour(supervisor).

-export([start/0, start_in_shell_for_testing/0, start_link/1, init/1]).

start() ->
    spawn(fun() ->
        supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = [])
    end),
    ok.

start_in_shell_for_testing() ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []),
    unlink(Pid),
    ok.

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(_Args) ->
    {ok, {{simple_one_for_one, 0, 1},
        [{stomp_client,
          {stomp_client, start_link, []},
          temporary, brutal_kill, worker, [stomp_client]}
        ]}}.
