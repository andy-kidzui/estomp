%% @author Andrew Kreiling <andy@kidzui.com>
%% @copyright 2008 KidZui, Inc. All Rights Reserved.

%% @doc gen_server

-module(stomp_client).
-author('Andrew Kreiling <andy@kidzui.com>').

-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% External exports
-export([start/1, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {parent, host, port, login, passcode, socket, buffer}).

%% @spec start(Args) -> ok
%% @doc Start a stomp_client.
start(Args) ->
    supervisor:start_child(stomp_supervisor, [[self(), Args]]).

%% @spec start_link(Args) -> Result
%% @doc gen_server start_link, callback for supervisor.
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%% @spec init(Args) -> {ok, State}
%% @doc gen_server init, starts the client in an initial state.
init([Parent, Args]) ->
    Host = proplists:get_value(host, Args),
    Port = proplists:get_value(port, Args),
    Login = proplists:get_value(login, Args),
    Passcode = proplists:get_value(passcode, Args),
    process_flag(trap_exit, true),
    link(Parent),
    State = #state{parent = Parent, host = Host, port = Port, login = Login, passcode = Passcode},
    {ok, State}.

%% @spec handle_call(Message, From, State) -> tuple()
%% @doc gen_server callback.
handle_call({stop}, _From, State) ->
    {stop, normal, State};

handle_call(_Message, _From, State) ->
    {reply, error, State}.

%% @spec handle_cast(Message, State) -> tuple()
%% @doc gen_server callback.
handle_cast(_Message, State) ->
    {noreply, State}.

%% @spec handle_info(Message, State) -> tuple()
%% @doc gen_server callback.
handle_info(_Message, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> ok
%% @doc gen_server termination callback.
terminate(_Reason, _State) ->
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, State}
%% @doc gen_server code_change callback (trivial).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-ifdef(EUNIT).
-endif.
