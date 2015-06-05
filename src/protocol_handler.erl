
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


get_msg_id(#'ProtocolTimeSync'{}) -> 1;
get_msg_id(#'ProtocolSceneInit'{}) -> 10;
get_msg_id(#'ProtocolSceneSync'{}) -> 11;
get_msg_id(#'ProtocolUnitAdd'{}) -> 100;
get_msg_id(#'ProtocolUnitUpdate'{}) -> 101;
get_msg_id(#'ProtocolUnitRemove'{}) -> 102;
get_msg_id(#'ProtocolDotAdd'{}) -> 200;
get_msg_id(#'ProtocolDotRemove'{}) -> 201.


get_msg_name(1) -> 'ProtocolTimeSync';
get_msg_name(10) -> 'ProtocolSceneInit';
get_msg_name(11) -> 'ProtocolSceneSync';
get_msg_name(100) -> 'ProtocolUnitAdd';
get_msg_name(101) -> 'ProtocolUnitUpdate';
get_msg_name(102) -> 'ProtocolUnitRemove';
get_msg_name(200) -> 'ProtocolDotAdd';
get_msg_name(201) -> 'ProtocolDotRemove'.

