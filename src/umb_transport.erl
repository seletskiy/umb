-module(umb_transport).
-created('Date: 06/02/2013').
-created_by('Stanislav Seletskiy <s.seletskiy@gmail.com>').

-behaviour(gen_server).

-export([
    connect/1,
    disconnect/1,
    start_transmission/1,
    end_transmission/1,
    send/2,
    recv/1,
    recv/2
]).

-export([
    start_link/3
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-record(state, {
    transport_module :: atom(),
    transport_state  :: term()
}).

start_link(Id, TransportName, TransportArgs) ->
    gen_server:start_link({local, Id}, ?MODULE,
        [TransportName, TransportArgs], []).

connect(Pid) ->
    gen_server:call(Pid, connect, infinity).

disconnect(Pid) ->
    gen_server:call(Pid, disconnect, infinity).

start_transmission(Pid) ->
    gen_server:call(Pid, start_transmission, infinity).

end_transmission(Pid) ->
    gen_server:call(Pid, end_transmission, infinity).

send(Pid, Data) ->
    gen_server:call(Pid, {send, Data}, infinity).

recv(Pid) ->
    recv(Pid, all).
recv(Pid, Length) ->
    gen_server:call(Pid, {recv, Length}, infinity).

%% @hidden
init([Module, Args]) ->
    {ok, #state{
        transport_module = Module,
        transport_state  = Module:init(Args)}}.

%% @hidden
handle_call(connect, _From, State) ->
    Module = State#state.transport_module,
    sl:debug("connecting"),
    case Module:connect(State#state.transport_state) of
        {ok, S} ->
            {reply, ok, State#state{transport_state = S}};
        Error ->
            sl:error("connection failed: ~p", [Error]),
            {reply, Error, State}
    end;

handle_call(disconnect, _From, State) ->
    Module = State#state.transport_module,
    sl:debug("disconnecting"),
    case Module:disconnect(State#state.transport_state) of
        {ok, S} ->
            {reply, ok, State#state{transport_state = S}};
        Error ->
            sl:error("disconnect failed: ~p", [Error]),
            {reply, Error, State}
    end;

handle_call(start_transmission, _From, State) ->
    Module = State#state.transport_module,
    sl:debug("starting transmission"),
    case Module:start_transmission(State#state.transport_state) of
        {ok, S} ->
            {reply, ok, State#state{transport_state = S}};
        Error ->
            sl:error("starting transmission failed: ~p", [Error]),
            {reply, Error, State}
    end;

handle_call(end_transmission, _From, State) ->
    Module = State#state.transport_module,
    sl:debug("ending transmission"),
    case Module:end_transmission(State#state.transport_state) of
        {ok, S} ->
            {reply, ok, State#state{transport_state = S}};
        Error ->
            sl:error("ending transmission failed: ~p", [Error]),
            {reply, Error, State}
    end;

handle_call({send, Data}, _From, State) ->
    Module = State#state.transport_module,
    sl:debug("send ~w", [Data]),
    case Module:send(Data, State#state.transport_state) of
        {ok, S} ->
            {reply, ok, State#state{transport_state = S}};
        Error ->
            sl:error("send failed: ~p", [Error]),
            {reply, Error, State}
    end;

handle_call({recv, Length}, _From, State) ->
    Module = State#state.transport_module,
    sl:debug("recv ~p", [Length]),
    case Module:recv(Length, State#state.transport_state) of
        {ok, Packet, S} ->
            sl:debug("received ~w", [Packet]),
            {reply, {ok, Packet}, State#state{transport_state = S}};
        Error ->
            sl:error("recv failed: ~p", [Error]),
            {reply, Error, State}
    end;

handle_call(_Message, _From, State) ->
    {noreply, State}.

%% @hidden
handle_cast(_Message, State) ->
    {noreply, State}.

%% @hidden
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
