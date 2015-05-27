
-module(protocol_handler).

-export([pack/1,
         pack_with_id/1,
         process/2]).


-include("protocol.hrl").

pack(Msg) ->
    protocol:encode_msg(Msg).

pack_with_id(Msg) ->
    Id = get_msg_id(Msg),
    Data = pack(Msg),
    <<Id:16, Data/binary>>.

process(DataWithId, State) ->
    <<Id:16, Data/binary>> = DataWithId,
    MsgName = get_msg_name(Id),
    DecodedData = protocol:decode_msg(Data, MsgName),
    protocol_implement:process(DecodedData, State).


get_msg_id(#'ProtocolUnitAdd'{}) -> 1;
get_msg_id(#'ProtocolUnitUpdate'{}) -> 2;
get_msg_id(#'ProtocolUnitRemove'{}) -> 3.


get_msg_name(1) -> 'ProtocolUnitAdd';
get_msg_name(2) -> 'ProtocolUnitUpdate';
get_msg_name(3) -> 'ProtocolUnitRemove'.

