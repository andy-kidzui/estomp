%% @author Andrew Kreiling <andy@kidzui.com>
%% @copyright 2008 KidZui, Inc. All Rights Reserved.

%% @doc gen_fsm

-module(stomp_client).
-author('Andrew Kreiling <andy@kidzui.com>').

-behaviour(gen_fsm).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% External exports
-export([start/1, start_link/1]).
-export([disconnect/1, subscribe/2, subscribe/3, ack/2, ack/3, send/3, unsubscribe/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([connecting/2, connecting/3, connect/2, connect/3, connected/2, connected/3]).

-record(state, {parent, host, port, login, passcode, socket, buffer}).

%% @spec start(Args) -> ok
%% @doc Start a stomp_client.
start(Args) ->
    supervisor:start_child(stomp_supervisor, [{self(), Args}]).

%% @spec start_link(Args) -> Result
%% @doc gen_fsm start_link, callback for supervisor.
start_link(Args) ->
    gen_fsm:start_link(?MODULE, Args, []).

%% @spec disconnect(FsmRef) -> Reply
%% @doc Stomp API - send a disconnect message
disconnect(FsmRef) ->
    gen_fsm:sync_send_event(FsmRef, {disconnect}).

%% @spec subscribe(FsmRef, Destination) -> Reply
%% @equiv subscribe(FsmRef, Destination, auto)
subscribe(FsmRef, Destination) ->
    subscribe(FsmRef, Destination, auto).

%% @spec subscribe(FsmRef, Destination, Ack) -> Reply
%% @doc Stomp API - subscribe to a destination
subscribe(FsmRef, Destination, Ack) ->
    gen_fsm:sync_send_event(FsmRef, {subscribe, Destination, Ack}).

%% @spec ack(FsmRef, ID) -> Reply
%% @doc Stomp API - send an ack message
ack(FsmRef, ID) ->
    gen_fsm:sync_send_event(FsmRef, {ack, ID}).

%% @spec ack(FsmRef, ID, Transaction) -> Reply
%% @doc Stomp API - send an ack message with transaction
ack(FsmRef, ID, Transaction) ->
    gen_fsm:sync_send_event(FsmRef, {ack, ID, Transaction}).

%% @spec send(FsmRef, Destination, Body) -> Reply
%% @doc Stomp API - send a message
send(FsmRef, Destination, Body) ->
    gen_fsm:sync_send_event(FsmRef, {send, Destination, Body}).

%% @spec unsubscribe(FsmRef, Destination) -> Reply
%% @doc Stomp API - unsubscribe from a destination
unsubscribe(FsmRef, Destination) ->
    gen_fsm:sync_send_event(FsmRef, {unsubscribe, Destination}).

%% @spec init(Args) -> {ok, State}
%% where
%%       Args       = {Parent, Options}
%%       Parent     = pid()
%%       Options    = [Opt]
%%       Opt        = {host, Host} |
%%                    {port, Port} |
%%                    {login, Login} |
%%                    {passcode, Passcode}
%% @doc gen_fsm init, starts the client in an initial state.
init({Parent, Options} = _Args) ->
    Host = proplists:get_value(host, Options),
    Port = proplists:get_value(port, Options),
    Login = proplists:get_value(login, Options),
    Passcode = proplists:get_value(passcode, Options),
    link(Parent),
    State = #state{parent = Parent, host = Host, port = Port, login = Login, passcode = Passcode},
    {ok, connecting, State, 0}.

%% @spec handle_event(Event, StateName, State) -> tuple()
%% @doc gen_fsm handle_event callback.
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%% @spec handle_sync_event(Event, From, StateName, State) -> tuple()
%% @doc gen_fsm handle_sync_event callback.
handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

%% @spec handle_info(Message, StateName, State) -> tuple()
%% @doc gen_fsm handle_info callback.
handle_info({tcp, _Socket, Bin}, connected, State) ->
    {ok, Frames, Buffer} = parse_stream(Bin, State#state.buffer),
    [ gen_server:cast(State#state.parent, {stomp, Command, Headers, Body}) || {Command, Headers, Body} <- Frames ],
    {next_state, connected, State#state{buffer = Buffer}};

handle_info({tcp, Socket, _Bin}, _StateName, State) ->
    gen_tcp:close(Socket),
    {next_state, connecting, State#state{socket = undefined}, 5000};

handle_info({tcp_closed, Socket}, _StateName, State) ->
    gen_tcp:close(Socket),
    {next_state, connecting, State#state{socket = undefined}, 5000};

handle_info({tcp_error, Socket, _Reason}, _StateName, State) ->
    gen_tcp:close(Socket),
    {next_state, connecting, State#state{socket = undefined}, 5000};

handle_info(Message, StateName, State) ->
    io:format("~n~p:handle_info: ~p ~p~n", [?MODULE, StateName, Message]),
    {next_state, StateName, State}.

%% @spec terminate(Reason, StateName, State) -> ok
%% @doc gen_fsm termination callback.
terminate(_Reason, _StateName, _State) ->
    ok.

%% @spec code_change(OldVsn, StateName, State, Extra) -> {ok, State}
%% @doc gen_fsm code_change callback.
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% @spec connecting(Event, State) -> tuple()
%% @doc gen_fsm callback.
connecting(timeout, State) ->
    case gen_tcp:connect(State#state.host, State#state.port,
			 [binary, {active, true}, {packet, 0}], 5000) of
        {ok, Socket} ->
	    {next_state, connect, State#state{socket = Socket}, 0};
        {error, Reason} ->
	    io:format("~n~p:connecting: failed for ~p:~p ~p~n",
		      [?MODULE, State#state.host, State#state.port, Reason]),
	    {next_state, connecting, State, 5000}
    end;

connecting(Event, State) ->
    io:format("~n~p:connecting/2: ~p~n", [?MODULE, Event]),
    {next_state, connecting, State, 5000}.

%% @spec connecting(Event, From, State) -> tuple()
%% @doc gen_fsm callback.
connecting(_Event, _From, State) ->
    {reply, {error, connecting}, connecting, State, 5000}.

%% @spec connect(Event, State) -> tuple()
%% @doc gen_fsm callback.
connect(timeout, State) ->
    Command = 'CONNECT',
    Headers = [{login, State#state.login}, {passcode, State#state.passcode}],
    ok = send_frame(State#state.socket, Command, Headers),
    {next_state, connected, State};

connect(Event, State) ->
    io:format("~n~p:connect/2: ~p~n", [?MODULE, Event]),
    {next_state, connect, State, 5000}.

%% @spec connect(Event, From, State) -> tuple()
%% @doc gen_fsm callback.
connect(_Event, _From, State) ->
    {reply, {error, connecting}, connect, State, 5000}.

%% @spec connected(Event, State) -> tuple()
%% @doc gen_fsm callback.
connected(Event, State) ->
    io:format("~n~p:connected/2: ~p~n", [?MODULE, Event]),
    {next_state, connected, State}.

%% @spec connected(Event, From, State) -> tuple()
%% where
%%       Event = {disconnect} |
%%               {subscribe, Destination, Ack} |
%%               {ack, MessageID} |
%%               {ack, MessageID, Transaction} |
%%               {send, Destination, Body} |
%%               {unsubscribe, Destination}
%% @doc gen_fsm callback.
connected({disconnect}, _From, State) ->
    Command = 'DISCONNECT',
    ok = send_frame(State#state.socket, Command),
    {reply, ok, connected, State};

connected({subscribe, Destination, Ack}, _From, State) ->
    Command = 'SUBSCRIBE',
    Headers = [{destination, Destination}, {ack, Ack}],
    ok = send_frame(State#state.socket, Command, Headers),
    {reply, ok, connected, State};

connected({ack, MessageID}, _From, State) ->
    Command = 'ACK',
    Headers = [{'message-id', MessageID}],
    ok = send_frame(State#state.socket, Command, Headers),
    {reply, ok, connected, State};

connected({ack, MessageID, Transaction}, _From, State) ->
    Command = 'ACK',
    Headers = [{'message-id', MessageID}, {'transaction', Transaction}],
    ok = send_frame(State#state.socket, Command, Headers),
    {reply, ok, connected, State};

connected({send, Destination, Body}, _From, State) ->
    Command = 'SEND',
    Headers = [{'destination', Destination}],
    ok = send_frame(State#state.socket, Command, Headers, Body),
    {reply, ok, connected, State};

connected({unsubscribe, Destination}, _From, State) ->
    Command = 'UNSUBSCRIBE', 
    Headers = [{destination, Destination}],
    ok = send_frame(State#state.socket, Command, Headers),
    {reply, ok, connected, State};

connected(Event, _From, State) ->
    io:format("~n~p:connected/3: ~p~n", [?MODULE, Event]),
    {reply, ok, connected, State}.

%%
%%

send_frame(Socket, Command) ->
    send_frame(Socket, Command, []).

send_frame(Socket, Command, Headers) ->
    send_frame(Socket, Command, Headers, []).

send_frame(Socket, Command, Headers, Body) ->
    Packet = generate_frame(Command, Headers, Body),
    gen_tcp:send(Socket, Packet).

generate_frame(Command, Headers, Body) ->
    [
     term_to_string(Command), $\n,
     [ [term_to_string(Key), $:, term_to_string(Value), $\n] || {Key, Value} <- Headers ], $\n,
     term_to_string(Body),
     $\0
    ].

term_to_string(Term) when is_float(Term) ->
    float_to_list(Term);
term_to_string(Term) when is_integer(Term) ->
    integer_to_list(Term);
term_to_string(Term) when is_atom(Term) ->
    atom_to_list(Term);
term_to_string(Term) ->
    Term.

parse_stream(Bin, undefined) ->
    parse_stream(Bin, {[], []});

parse_stream(Bin, {FrameBuffer, LineBuffer}) ->
    parse_stream(Bin, [], FrameBuffer, LineBuffer).

parse_stream(<<$\r, Bin/binary>>, Frames, FrameBuffer, LineBuffer) ->
    parse_stream(Bin, Frames, FrameBuffer, LineBuffer);

parse_stream(<<$\n, Bin/binary>>, Frames, FrameBuffer, LineBuffer) ->
    Line = lists:reverse(LineBuffer),
    parse_stream(Bin, Frames, [Line|FrameBuffer], []);

parse_stream(<<$\0, Bin/binary>>, Frames, FrameBuffer, LineBuffer) ->
    Line = lists:reverse(LineBuffer),
    Frame = parse_frame(lists:reverse([Line|FrameBuffer])),
    parse_stream(Bin, [Frame|Frames], [], []);

parse_stream(<<Char, Bin/binary>>, Frames, FrameBuffer, LineBuffer) ->
    parse_stream(Bin, Frames, FrameBuffer, [Char|LineBuffer]);

parse_stream(<<>>, Frames, FrameBuffer, LineBuffer) ->
    {ok, Frames, {FrameBuffer, LineBuffer}}.

parse_frame(Frame) ->
    parse_frame(Frame, undefined, undefined, undefined).

parse_frame([[]|T], undefined, _, _) ->
    parse_frame(T, undefined, undefined, undefined);

parse_frame([H|T], undefined, _, _) ->
    parse_frame(T, list_to_atom(H), [], undefined);

parse_frame([[]|T], Command, Headers, undefined) ->
    parse_frame(T, Command, Headers, []);

parse_frame([H|T], Command, Headers, undefined) ->
    {ok, Header} = parse_key_value_pair(H),
    parse_frame(T, Command, [Header|Headers], undefined);

parse_frame([H|T], Command, Headers, []) ->
    parse_frame(T, Command, Headers, [H]);

parse_frame([H|T], Command, Headers, Body) ->
    parse_frame(T, Command, Headers, [H, $\n|Body]);

parse_frame([], Command, Headers, Body) ->
    {Command, Headers, lists:flatten(lists:reverse(Body))}.

parse_key_value_pair(String) ->
    Pos = string:chr(String, $:),
    {ok, {
       list_to_atom(string:strip(string:substr(String, 1, Pos - 1))),
       string:strip(string:substr(String, Pos + 1 ))
      }}.

-ifdef(EUNIT).
test1_test() ->
    Bin = <<"CONNECTED\nsession: test\n\n\0">>,
    Expect = {ok,[{'CONNECTED',[{session, "test"}], []}],{[],[]}},
    ?assertEqual(parse_stream(Bin, undefined), Expect).

test2_test() ->
    Bin = <<"\n\n\n\n\nCONNECTED\nsession: test\n\n\0">>,
    Expect = {ok,[{'CONNECTED',[{session, "test"}], []}],{[],[]}},
    ?assertEqual(parse_stream(Bin, undefined), Expect).

test3_test() ->
    Bin = <<"CONNECTED\nsession: test\n\nthis is a test of the body\0">>,
    Expect = {ok,[{'CONNECTED',[{session, "test"}], "this is a test of the body"}],{[],[]}},
    ?assertEqual(parse_stream(Bin, undefined), Expect).

test4_test() ->
    Bin = <<"CONNECTED\nsession: test\n\nthis is a test of the body\nwith multiple lines\0">>,
    Expect = {ok,[{'CONNECTED',[{session, "test"}], "this is a test of the body\nwith multiple lines"}],{[],[]}},
    ?assertEqual(parse_stream(Bin, undefined), Expect).

test5_test() ->
    Bin = <<"CONNECTED\nsession: test\n\nthis is a test of the body\nwith multiple lines\n\0">>,
    Expect = {ok,[{'CONNECTED',[{session, "test"}], "this is a test of the body\nwith multiple lines\n"}],{[],[]}},
    ?assertEqual(parse_stream(Bin, undefined), Expect).

test6_test() ->
    Bin1 = <<"CONNECTED\nsession: test\n\nthis is a ">>,
    Bin2 = <<"test of the body\nwith multiple lines\n\0">>,
    Expect = {ok,[{'CONNECTED',[{session, "test"}], "this is a test of the body\nwith multiple lines\n"}],{[],[]}},
    {ok, [], Buffer} = parse_stream(Bin1, undefined),
    ?assertEqual(parse_stream(Bin2, Buffer), Expect).
-endif.
