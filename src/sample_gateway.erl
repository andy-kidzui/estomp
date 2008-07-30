%% @author Andrew Kreiling <andy@kidzui.com>
%% @copyright 2008 KidZui, Inc. All Rights Reserved.

%% @doc Sample stomp gateway.

-module(sample_gateway).
-author('Andrew Kreiling <andy@kidzui.com>').

-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% External exports
-export([start/1, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {client}).

start(Args) ->
    spawn(fun() ->
		  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, [])
	  end),
    ok.

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init(Args) ->
    process_flag(trap_exit, true),
    {ok, Pid} = stomp_client:start(Args),
    State = #state{client = Pid},
    {ok, State}.

handle_call(_Message, _From, State) ->
    {reply, error, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, Reason}, #state{client = Pid} = State) ->
    {stop, Reason, State};

handle_info(_Message, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

-ifdef(EUNIT).
-endif.
