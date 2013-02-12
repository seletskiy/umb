%% @author Stanislav Seletskiy <s.seletskiy@gmail.com>
%% @doc Module calculates CRC16-CITT sum.
-module(crc16_citt).
-created('Date: 07/02/2013').
-created_by('Stanislav Seletskiy <s.seletskiy@gmail.com>').

-export([crc/1]).

-define(START, 16#FFFF).
-define(POLY,  16#8408).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Calculates CRC16-CITT sum with init vector 16#FFFF and poly 16#8408.
-spec crc(binary()) -> integer().
crc(Binary) ->
    crc_binary(Binary, ?START).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

crc_binary(<<Byte:8, Tail/bitstring>>, Crc) ->
    crc_binary(Tail, crc_byte(8, Byte, Crc));
crc_binary(<<>>, Crc) ->
    Crc.

crc_byte(0, 0, Crc) ->
    Crc;
crc_byte(N, Byte, Crc) ->
    Crc2 = case (Crc band 1) bxor (Byte band 1) of
        0 -> (Crc bsr 1);
        1 -> (Crc bsr 1) bxor ?POLY
    end,
    crc_byte(N - 1, Byte bsr 1, Crc2).
