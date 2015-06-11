
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


get_msg_id(#'ProtocolConfig'{}) -> 1;
get_msg_id(#'ProtocolTimeSync'{}) -> 2;
get_msg_id(#'ProtocolUnitCreate'{}) -> 100;
get_msg_id(#'ProtocolUnitAdd'{}) -> 101;
get_msg_id(#'ProtocolUnitMove'{}) -> 102;
get_msg_id(#'ProtocolUnitSplit'{}) -> 103;
get_msg_id(#'ProtocolUnitEject'{}) -> 104;
get_msg_id(#'ProtocolUnitRemove'{}) -> 105;
get_msg_id(#'ProtocolDotRemove'{}) -> 200;
get_msg_id(#'ProtocolSceneInit'{}) -> 1000;
get_msg_id(#'ProtocolSceneSync'{}) -> 1001.


get_msg_name(1) -> 'ProtocolConfig';
get_msg_name(2) -> 'ProtocolTimeSync';
get_msg_name(100) -> 'ProtocolUnitCreate';
get_msg_name(101) -> 'ProtocolUnitAdd';
get_msg_name(102) -> 'ProtocolUnitMove';
get_msg_name(103) -> 'ProtocolUnitSplit';
get_msg_name(104) -> 'ProtocolUnitEject';
get_msg_name(105) -> 'ProtocolUnitRemove';
get_msg_name(200) -> 'ProtocolDotRemove';
get_msg_name(1000) -> 'ProtocolSceneInit';
get_msg_name(1001) -> 'ProtocolSceneSync'.

