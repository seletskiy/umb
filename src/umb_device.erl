%% @author Stanislav Seletskiy <s.seletskiy@gmail.com>
%% @doc Represents device in communication scheme.
-module(umb_device).
-created('Date: 07/02/2013').
-created_by('Stanislav Seletskiy <s.seletskiy@gmail.com>').

-export([
    new/1,
    new/2,
    addr/1
]).

-record(device, {
    class :: integer(),
    identifier :: integer()
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new(Address) ->
    #device{class = Address bsr 12, identifier = Address band 2#111111111111}.
new(ClassId, DeviceId) ->
    #device{class = ClassId, identifier = DeviceId}.

addr(#device{class = ClassId, identifier = DeviceId}) ->
    ClassId bsl 12 + DeviceId.
