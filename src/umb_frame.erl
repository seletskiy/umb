-module(umb_frame).
-created('Date: 07/02/2013').
-created_by('Stanislav Seletskiy <s.seletskiy@gmail.com>').

-include("umb.hrl").

-export([
    new/0,
    set_to/2,
    set_from/2,
    set_cmd/2,
    cmd/1,
    set_payload/2,
    payload/1,
    encode/1,
    code_int/1,
    code_long/1,
    code_float/1,
    code_double/1,
    decode/1
]).

-record(frame, {
    ver     = ?UMB_VER_1_0 :: integer(),
    to                     :: integer(),
    from                   :: integer(),
    cmd                    :: integer(),
    verc    = ?UMB_VER_1_0 :: integer(),
    payload = <<>>         :: binary(),
    cs                     :: integer()
}).

new() ->
    #frame{}.
    
set_to(Frame, Device) ->
    Frame#frame{to = Device}.

set_from(Frame, Device) ->
    Frame#frame{from = Device}.

set_cmd(Frame, Cmd) ->
    Frame#frame{cmd = Cmd}.

cmd(Frame) ->
    Frame#frame.cmd.

set_payload(Frame, Payload) ->
    Frame#frame{payload = Payload}.

payload(Frame) ->
    Frame#frame.payload.

encode(Frame) ->
    To   = code_int(umb_device:addr(Frame#frame.to)),
    From = code_int(umb_device:addr(Frame#frame.from)),
    Data = <<?UMB_FRM_SOH:8,
        (Frame#frame.ver):8,
        To:16,
        From:16,
        (2 + byte_size(Frame#frame.payload)):8,
        ?UMB_FRM_STX:8,
        (Frame#frame.cmd):8,
        (Frame#frame.verc):8,
        (Frame#frame.payload)/binary,
        ?UMB_FRM_ETX:8>>,
    Crc = crc16_citt:crc(Data),
    <<Data/binary, (code_int(Crc)):16, ?UMB_FRM_EOT:8>>.

decode(Source) ->
    decode(soh, new(), Source(1), Source).
decode(State, _, {error, Reason}, _Source) ->
    {error, {source_error, Reason, State}};
decode(soh, Frame, {ok, <<?UMB_FRM_SOH:8>>}, Source) ->
    decode(ver, Frame, Source(1), Source);
decode(soh, _, _, _) ->
    {error, {malformed_frame, soh}};
decode(ver, Frame, {ok, <<?UMB_VER_1_0:8>>}, Source) ->
    decode(to, Frame, Source(2), Source);
decode(ver, _, Ver, _) ->
    {error, {unsupported_frame, {ver, Ver}}};
decode(to, Frame, {ok, <<Addr:16>>}, Source) ->
    Frame2 = set_to(Frame, umb_device:new(code_int(Addr))),
    decode(from, Frame2, Source(2), Source);
decode(from, Frame, {ok, <<Addr:16>>}, Source) ->
    Frame2 = set_from(Frame, umb_device:new(code_int(Addr))),
    decode(len, Frame2, Source(1), Source);
decode(len, Frame, {ok, <<Len:8>>}, Source) ->
    BinLen = (Len - 2) * 8,
    Frame2 = set_payload(Frame, <<0:BinLen>>),
    decode(stx, Frame2, Source(1), Source);
decode(stx, Frame, {ok, <<?UMB_FRM_STX>>}, Source) ->
    decode(cmd, Frame, Source(1), Source);
decode(stx, _, _, _) ->
    {error, {malformed_frame, stx}};
decode(cmd, Frame, {ok, <<Cmd:8>>}, Source) ->
    Frame2 = set_cmd(Frame, Cmd),
    decode(verc, Frame2, Source(1), Source);
decode(verc, Frame, {ok, <<?UMB_VER_1_0:8>>}, Source) ->
    Len = byte_size(payload(Frame)),
    decode(payload, Frame, Source(Len), Source);
decode(verc, _, {ok, <<Ver:8>>}, _) ->
    {error, {unsupported_frame, {verc, Ver}}};
decode(payload, Frame, {ok, Payload}, Source) ->
    Frame2 = set_payload(Frame, Payload),
    decode(etx, Frame2, Source(1), Source);
decode(etx, Frame, {ok, <<?UMB_FRM_ETX:8>>}, Source) ->
    decode(crc, Frame, Source(2), Source);
decode(etx, _, _, _) ->
    {error, {malformed_frame, etx}};
decode(crc, Frame, {ok, <<Crc:16>>}, Source) ->
    DataBitLen = 88 + bit_size(payload(Frame)),
    <<_:DataBitLen, TrueCrc:16, _/binary>> = encode(Frame),
    case Crc of
        TrueCrc ->
            decode(eot, Frame, Source(1), Source);
        _ ->
            {error, {malformed_frame, {crc, TrueCrc, Crc}}}
    end;
decode(eot, Frame, {ok, <<?UMB_FRM_EOT:8>>}, _Source) ->
    {ok, Frame};
decode(eot, _, _, _) ->
    {error, {malformed_frame, eot}}.

code_int(Int) ->
    <<Result:16>> = <<Int:16/little>>,
    Result.

code_long(Long) ->
    <<Result:32>> = <<Long:32/little>>,
    Result.

code_float(Float) ->
    <<Result:32/float-little>> = <<Float:32>>,
    Result.

code_double(Double) ->
    <<Result:64/float-little>> = <<Double:64>>,
    Result.
