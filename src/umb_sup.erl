-module(umb_sup).
-behaviour(supervisor).

-export([
    start_bus/3
]).

-export([
    start_link/0,
    init/1
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    {ok, {{one_for_one, 5, 60}, []}}.

start_bus(Id, TransportName, TransportArgs) ->
    case start_transport(Id, TransportName, TransportArgs) of
        Error = {error, _Reason} ->
            Error;
        {ok, Connect} ->
            Result = supervisor:start_child(?MODULE,
                {umb_bus,
                    {umb_bus, start_link, [Id, Connect]},
                    permanent,
                    brutal_kill,
                    worker,
                    [umb_bus]}),
            case Result of
                {ok, _Pid} ->
                    {ok, Id};
                Error = {error, _Reason} ->
                    Error
            end
    end.

start_transport(BusId, TransportName, TransportArgs) ->
    Id = list_to_atom(lists:flatten(io_lib:format("~s_transport", [BusId]))),
    Result = supervisor:start_child(?MODULE,
        {umb_transport,
            {umb_transport, start_link, [Id, TransportName, TransportArgs]},
            permanent,
            brutal_kill,
            worker,
            [umb_transport, TransportName]}),
    case Result of
        {ok, _Pid} ->
            {ok, Id};
        Error = {error, _Reason} ->
            Error
    end.
