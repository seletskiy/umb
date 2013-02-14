-module(umb_request).
-created('Date: 07/02/2013').
-created_by('Stanislav Seletskiy <s.seletskiy@gmail.com>').

-include("umb.hrl").

-export([
    version/0,
    info/1,
    info/2,
    info_cnl_num/0,
    info_cnl_list/1,
    info_cnl_full/1,
    r2s_litres_since/0,
	r2s_precipation_type/0,
	r2s_snowflakes/0,
    online_data/1,
	multichannel/1,
    payload/1
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

version() ->
    umb_frame:set_cmd(umb_frame:new(), ?UMB_CMD_VERSION).

info(Info) ->
    Frame1 = umb_frame:set_cmd(umb_frame:new(), ?UMB_CMD_INFO),
    Frame2 = umb_frame:set_payload(Frame1, <<Info:8>>),
    Frame2.

info(Info, Option) ->
    Frame1 = info(Info),
    Frame2 = umb_frame:set_payload(Frame1, <<
        (umb_frame:payload(Frame1))/binary, Option/binary>>),
    Frame2.

info_cnl_num() ->
    info(?UMB_INF_CNL_NUM).

info_cnl_list(Block) ->
    info(?UMB_INF_CNL_LIST, <<Block:8>>).

info_cnl_full(Channel) ->
    info(?UMB_INF_CNL_FULL, <<(umb_frame:code_int(Channel)):16>>).

r2s_litres_since() ->
	online_data(?UMB_CNL_R2S_LITRES_SINCE).

r2s_precipation_type() ->
	online_data(?UMB_CNL_R2S_PREC_TYPE).

r2s_snowflakes() ->
	online_data(?UMB_CNL_R2S_SFLAKES).

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
    {ok, {HWVer, SWVer}};
decode_payload(?UMB_CMD_VERSION, _) ->
    {error, {malformed_payload, {version, invalid_version}}};
decode_payload(?UMB_CMD_INFO, <<0:8, ?UMB_INF_CNL_NUM, Cnl:16, Blocks:8>>) ->
    {ok, {umb_frame:code_int(Cnl), Blocks}};
decode_payload(?UMB_CMD_INFO, <<0:8, ?UMB_INF_CNL_LIST, _B:8, Count:8,
        Tail/binary>>) ->
    decode_payload_cnl_list(Count, Tail);
decode_payload(?UMB_CMD_INFO, <<0:8, ?UMB_INF_CNL_FULL, Tail/binary>>) ->
    decode_payload_cnl_full(Tail);
decode_payload(?UMB_CMD_INFO, _) ->
    {error, unknown_info_answer};
decode_payload(?UMB_CMD_ONLINE_DATA, <<0:8, Cnl:16, Type:8, Val/binary>>) ->
    Channel = umb_frame:code_int(Cnl),
    decode_payload_cnl(Channel, Type, Val);
decode_payload(?UMB_CMD_ONLINE_DATA, _) ->
    {error, {malformed_payload, online_data}};
decode_payload(?UMB_CMD_MULTICHANNEL, <<0:8, Count:8, BinTail/binary>>) ->
    decode_multichannel(Count, BinTail);
decode_payload(_, _) ->
	{error, unknown_payload_type}.

decode_payload_cnl_list(Count, Binary) ->
    decode_payload_cnl_list(Count, Binary, []).
decode_payload_cnl_list(0, <<>>, Result) ->
    {ok, lists:reverse(Result)};
decode_payload_cnl_list(0, Rest, _) ->
    {error, {invalid_amount_of_channels, {rest, Rest}}};
decode_payload_cnl_list(Count, <<>>, _) ->
    {error, invalid_amount_of_channels, {count, Count}};
decode_payload_cnl_list(Count, <<Cnl:16, Tail/binary>>, Result) ->
    decode_payload_cnl_list(Count - 1, Tail, [
        umb_frame:code_int(Cnl) | Result]);
decode_payload_cnl_list(_, _, _) ->
    {error, malformed_channel_spec}.

decode_payload_cnl_full(<<Cnl:16, Var:160/bitstring, Unit:120/bitstring, Mv:8,
        Type:8, Tail/binary>>) ->
    DataBitSize = trunc(bit_size(Tail) / 2),
    <<BinMin:DataBitSize/bitstring, BinMax:DataBitSize/bitstring>> = Tail,
    case [decode_data_value(Type, BinMin), decode_data_value(Type, BinMax)] of
        [{ok, Min}, {ok, Max}] -> {ok, [
                {channel, umb_frame:code_int(Cnl)},
                {variable, string:strip(binary_to_list(Var))},
                {unit, string:strip(binary_to_list(Unit))},
                {mv_type, Mv},
                {data_type, decode_data_type(Type)},
                {min, Min},
                {max, Max}]};
        _ ->
            {error, malformed_data_range}
    end.

decode_payload_cnl(?UMB_CNL_R2S_PREC_TYPE, Type, Val) ->
    case decode_data_value(Type, Val) of
        {ok, 0 } -> {ok, none};
        {ok, 60} -> {ok, rain};
        {ok, 67} -> {ok, frz_rain};
        {ok, 69} -> {ok, sleet};
        {ok, 70} -> {ok, snow};
        {ok, 90} -> {ok, hail};
        {ok, _X} -> {ok, ufo};
        Error    -> Error
    end;
decode_payload_cnl(_Channel, Type, Val) ->
	decode_data_value(Type, Val).

decode_multichannel(Count, BinTail) ->
    decode_multichannel(Count, BinTail, []).
decode_multichannel(0, <<>>, Data) ->
    {ok, lists:reverse(Data)};
decode_multichannel(_, <<>>, _) ->
    {error, unexpected_end_of_binary};
decode_multichannel(0, _, _) ->
    {error, invalid_number_of_channels};
decode_multichannel(Count, <<SubLen:8, BinTail/binary>>, Result) ->
    BitSubLen = SubLen * 8,
    <<Telegram:BitSubLen/bitstring, Rest/binary>> = BinTail,
    case decode_payload(?UMB_CMD_ONLINE_DATA, Telegram) of
        {ok, Item} ->
            decode_multichannel(Count - 1, Rest, [Item | Result]);
        Error ->
            Error
    end.

decode_data_type(Type) ->
    case Type of
        ?UMB_DATA_UCHAR  -> uchar;
        ?UMB_DATA_CHAR   -> char;
        ?UMB_DATA_USHORT -> ushort;
        ?UMB_DATA_SHORT  -> short;
        ?UMB_DATA_ULONG  -> ulong;
        ?UMB_DATA_LONG   -> long;
        ?UMB_DATA_FLOAT  -> float;
        ?UMB_DATA_DOUBLE -> double
    end.
decode_data_value(?UMB_DATA_UCHAR, <<Char:8>>) ->
	{ok, Char};
decode_data_value(?UMB_DATA_CHAR, <<Char:8>>) ->
	{ok, Char};
decode_data_value(?UMB_DATA_USHORT, <<Short:16>>) ->
	{ok, umb_frame:code_int(Short)};
decode_data_value(?UMB_DATA_SHORT, <<Short:16>>) ->
	{ok, umb_frame:code_int(Short)};
decode_data_value(?UMB_DATA_ULONG, <<Long:32>>) ->
	{ok, umb_frame:code_long(Long)};
decode_data_value(?UMB_DATA_LONG, <<Long:32>>) ->
	{ok, umb_frame:code_long(Long)};
decode_data_value(?UMB_DATA_FLOAT, <<Float:32>>) ->
    {ok, umb_frame:code_float(Float)};
decode_data_value(?UMB_DATA_DOUBLE, <<Double:64>>) ->
    {ok, umb_frame:code_double(Double)};
decode_data_value(Type, Value) ->
	{error, {could_not_decode, Type, Value}}.
