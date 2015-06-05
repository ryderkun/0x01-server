
-module(protocol_implement).

-include("protocol.hrl").
-include("player.hrl").

-export([process/2]).

process(#'ProtocolTimeSync'{} = Msg, State) ->
    %% The first message send from client
    NewMsg = Msg#'ProtocolTimeSync'{server = eatrun_utils:timestamp_in_milliseconds()},
    Data = protocol_handler:pack_with_id(NewMsg),
    self() ! {notify, Data},
    {ok, RoomPid} = eatrun_room_server:join_room(self()),
    State#player{roompid = RoomPid};


process(#'ProtocolUnitAdd'{units = ProtocolUnits} = MsgUnitAdd, #player{ids = Ids, roompid = RoomPid} = State) ->
    NewIds = lists:foldl(
        fun(U, Acc) -> gb_sets:add_element(U#'ProtocolUnit'.id, Acc) end,
        Ids,
        ProtocolUnits
    ),

    Units = eatrun_utils:protocol_units_to_server_units(ProtocolUnits),
    DataUnitAdd = protocol_handler:pack_with_id(MsgUnitAdd),
    gen_server:cast(RoomPid, {unitadd, Units, DataUnitAdd, self()}),
    State#player{ids = NewIds};


process(#'ProtocolUnitUpdate'{milliseconds = Ms, units = PUnits}, #player{roompid = RoomPid} = State) ->
    gen_server:cast(RoomPid, {unitupdate, PUnits, Ms}),
    State;


process(#'ProtocolUnitRemove'{}, State) ->
    erlang:error("Not Implement: ProtocolUnitRemove"),
    State;


process(#'ProtocolDotRemove'{ids = Ids}, #player{roompid = RoomId} = State) ->
    gen_server:cast(RoomId, {dotremove, Ids}),
    State.
