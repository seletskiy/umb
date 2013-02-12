-module(umb_request).
-created('Date: 07/02/2013').
-created_by('Stanislav Seletskiy <s.seletskiy@gmail.com>').

-include("umb.hrl").

-export([
    version/0,
    r2s_litres_since/0,
	r2s_precipation_type/0,
	r2s_snowflakes/0,
	multichannel/1,
    payload/1
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

version() ->
    umb_frame:set_cmd(umb_frame:new(), ?UMB_CMD_VERSION).

r2s_litres_since() ->
	online_data(?UMB_CNL_R2S_LITRES_SINCE).

r2s_precipation_type() ->
	online_data(?UMB_CNL_R2S_PREC_TYPE).

r2s_snowflakes() ->
	online_data(?UMB_CNL_R2S_SNOWFLAKES).

online_data(Channel) ->
    Frame1 = umb_frame:new(),
    Frame2 = umb_frame:set_cmd(Frame1, ?UMB_CMD_ONLINE_DATA),
    Frame3 = umb_frame:set_payload(Frame2, channel(Channel)),
    Frame3.

multichannel(Frames) ->
	ResultFrame = umb_frame:new(),
	ResultFrame2 = umb_frame:set_cmd(ResultFrame, ?UMB_CMD_MULTICHANNEL),
	ResultFrame3 = umb_frame:set_payload(ResultFrame2, <<(length(Frames)):8>>),
    multichannel(Frames, ResultFrame3).

multichannel([], ResultFrame) ->
    ResultFrame;
multichannel([Frame | Tail], ResultFrame) ->
	ResultFrame2 = umb_frame:set_payload(ResultFrame, <<
		(umb_frame:payload(ResultFrame))/binary,
		(umb_frame:payload(Frame))/binary>>),
    multichannel(Tail, ResultFrame2).

payload(Frame) ->
    decode_payload(umb_frame:cmd(Frame), umb_frame:payload(Frame)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

channel(Channel) ->
    <<(umb_frame:code_int(Channel)):16>>.

decode_payload(?UMB_CMD_VERSION, <<0:8, HWVer:8, SWVer:8>>) ->
    {HWVer, SWVer};
decode_payload(?UMB_CMD_VERSION, _) ->
    {error, {malformed_payload, {version, invalid_version}}};
decode_payload(?UMB_CMD_ONLINE_DATA, <<0:8, _:16, Type:8, Val/binary>>) ->
	decode_data_type(Type, Val);
decode_payload(?UMB_CMD_ONLINE_DATA, _) ->
    {error, {malformed_payload, online_data}};

%decode_payload(?UMB_CMD_MULTICHANNEL, <<0:8, Count:8, BinTail/binary>>) ->
%    decode_multichannel(Count, BinTail).
decode_payload(_, _) ->
	{error, unknown_payload_type}.

decode_data_type(?UMB_DATA_UCHAR, <<Char:8>>) ->
	{ok, Char};
decode_data_type(?UMB_DATA_CHAR, <<Char:8>>) ->
	{ok, Char};
decode_data_type(?UMB_DATA_USHORT, <<Short:16>>) ->
	{ok, umb_frame:code_int(Short)};
decode_data_type(?UMB_DATA_SHORT, <<Short:16>>) ->
	{ok, umb_frame:code_int(Short)};
decode_data_type(?UMB_DATA_ULONG, <<Long:32>>) ->
	{ok, umb_frame:code_long(Long)};
decode_data_type(?UMB_DATA_LONG, <<Long:32>>) ->
	{ok, umb_frame:code_long(Long)};
decode_data_type(?UMB_DATA_FLOAT, <<Float:32>>) ->
	{ok, Float};
decode_data_type(?UMB_DATA_DOUBLE, <<Double:64>>) ->
	{ok, Double};
decode_data_type(Type, Value) ->
	{error, {could_not_decode, Type, Value}}.
