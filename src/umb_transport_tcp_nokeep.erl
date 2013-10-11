-module(umb_transport_tcp_nokeep).
-created('Date: 06/02/2013').
-created_by('Stanislav Seletskiy <s.seletskiy@gmail.com>').

-export([
    init/1,
    connect/1,
    disconnect/1,
    start_transmission/1,
    end_transmission/1,
    send/2,
    recv/2
]).

init(Args) ->
    sl:info("tcp no keep-alive extension"),
    umb_transport_tcp:init(Args).

connect(_State) ->
    {error, no_keep_alive_connection}.

disconnect(_State) ->
    {error, no_keep_alive_connection}.

start_transmission(State) ->
    umb_transport_tcp:connect(State).

end_transmission(State) ->
    umb_transport_tcp:disconnect(State).

send(Data, State) ->
    umb_transport_tcp:send(Data, State).

recv(Length, State) ->
    umb_transport_tcp:recv(Length, State).
