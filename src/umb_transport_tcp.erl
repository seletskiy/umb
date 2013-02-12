-module(umb_transport_tcp).
-created('Date: 06/02/2013').
-created_by('Stanislav Seletskiy <s.seletskiy@gmail.com>').

-export([
    init/1,
    connect/1,
    disconnect/1,
    send/2,
    recv/2
]).

-record(state, {
    host :: list(),
    port :: integer(),
    opts :: list(),
    sock
}).

init([Host, Port, Opts]) ->
    sl:info("tcp transport init"),
    Timeout = proplists:get_value(recv_timeout, Opts, infinity),
    sl:info("recv timeout set to: ~p", [Timeout]),
    #state{
        host = Host,
        port = Port,
        opts = Opts,
        sock = nil
    }.

connect(State = #state{host = Host, port = Port, opts = Opts}) ->
    sl:info("connecting (tcp) to ~s:~B", [Host, Port]),
    DefOpts = [{active, false}, {mode, binary}],
    TcpOpts = proplists:get_value(inet, Opts, []),
    case gen_tcp:connect(Host, Port, DefOpts ++ TcpOpts) of
        {ok, Sock} -> {ok, State#state{sock = Sock}};
        Error -> Error
    end.

disconnect(State = #state{sock = Sock}) ->
    case gen_tcp:close(Sock) of
        ok -> {ok, State#state{sock = nil}};
        Error -> Error
    end.

send(Data, State = #state{sock = Sock}) ->
    case gen_tcp:send(Sock, Data) of
        ok -> {ok, State};
        Error -> Error
    end.

recv(all, State) ->
    recv(0, State);
recv(Length, State = #state{sock = Sock, opts = Opts}) when is_integer(Length) ->
    Timeout = proplists:get_value(recv_timeout, Opts, infinity),
    case gen_tcp:recv(Sock, Length, Timeout) of
        {ok, Packet} -> {ok, Packet, State};
        Error -> Error
    end.
