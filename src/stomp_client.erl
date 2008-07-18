-module(stomp_client).
-author('Andrew Kreiling <andy@kidzui.com>').
-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([start/1, start_link/1, init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

start(Args) ->
    spawn(fun() ->
        supervisor:start_child(stomp_supervisor, [[self()] ++ Args])
    end),
    ok.

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(_Args) ->
    process_flag(trap_exit, true),
    State = #state{},
    {ok, State}.

handle_call(_Message, _From, State) ->
    {reply, error, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Message, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

-ifdef(EUNIT).
-endif.
