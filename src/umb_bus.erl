%% @author Stanislav Seletskiy <s.seletskiy@gmail.com>
%% @doc Frontend module that represents bus between devices.
%% 
%% There is basically two methods for user to use: {@see connect/3} and
%% {@see request/4}.
-module(umb_bus).
-created('Date: 07/02/2013').
-created_by('Stanislav Seletskiy <s.seletskiy@gmail.com>').

-behaviour(gen_server).

-export([
    connect/3,
    request/4
]).

-export([
    start_link/2
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-record(state, {
    connect :: pid()
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% End User Api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Connect to device bus.
%%
%% `Id` must be any unique bus id just for identifying bus with next requests.
%% `TransportName` must be a module name, that provides transport level for bus.
%% `TransportArgs` is a arbitary args, passed to transport module.
%%
%% For example of transport module {@see umb_transport_tcp}.
-spec connect(Id :: atom(), atom(), list()) -> {ok, Id :: atom()}.
connect(Id, TransportName, TransportArgs) ->
    umb_sup:start_bus(Id, TransportName, TransportArgs).

%% @doc Send request to bus and return answer.
%%
%% `BusId` is a `NewBusId`, passed to `connect/3`.
%% `From` is a device, that sends request ({@see umb_device}).
%% `To` is a device, that is a target of request.
%% `Frame` is a constructed frame for a request. You should use
%% {@see umb_request} for constructing such frames.
-spec request(BusId :: atom(), From :: umb_device:device(),
    To :: umb_device:device(), Frame :: umb_frame:frame()) ->
        {ok, Result :: term()} |
        {error, Reason ::term()}.
request(Bus, From, To, Frame) ->
    gen_server:call(Bus, {request, {From, To, Frame}}, infinity).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generic Server Specific
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
-spec start_link(atom(), atom()) -> {ok, pid()} | {error, Reason :: term()}.
start_link(Id, ConnId) ->
    gen_server:start_link({local, Id}, ?MODULE, [ConnId], []).


%% @hidden
init([ConnId]) ->
    {ok, #state{connect = ConnId}}.

%% @hidden
handle_call({request, {From, To, Frame}}, _From, State) ->
    Frame2 = umb_frame:set_to(Frame, To),
    Frame3 = umb_frame:set_from(Frame2, From),
    ConnId = State#state.connect,
    umb_transport:connect(ConnId),
    umb_transport:send(ConnId, umb_frame:encode(Frame3)),
    Source = fun(Amount) -> umb_transport:recv(ConnId, Amount) end,
    Reply = case umb_frame:decode(Source) of
        {ok, ReplyFrame} ->
            sl:debug("frame decoded successfully"),
            {ok, ReplyFrame};
        {error, Reason} ->
            sl:error("failed to decode packet: ~p", [Reason]),
            {error, Reason}
    end,
    umb_transport:disconnect(ConnId),
    {reply, Reply, State};
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
