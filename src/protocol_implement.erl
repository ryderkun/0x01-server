
-module(protocol_implement).

-include("protocol.hrl").
-include("player.hrl").

-export([process/2]).

process(#'ProtocolTimeSync'{} = Msg, State) ->
    NewMsg = Msg#'ProtocolTimeSync'{server = eatrun_utils:timestamp_in_milliseconds()},
    Data = protocol_handler:pack_with_id(NewMsg),
    self() ! {notify, Data},
    State;

process(#'ProtocolUnitAdd'{units = PUnits} = Msg, #player{ids = Ids, roompid = RoomPid} = State) ->
    NewIds = lists:foldl(
        fun(U, Acc) -> gb_sets:add_element(U#'ProtocolUnit'.id, Acc) end,
        Ids,
        PUnits
    ),

    Units = eatrun_utils:protocol_units_to_server_units(PUnits),
    gen_server:cast(RoomPid, {unitadd, Units, Msg, self()}),
    State#player{ids = NewIds};


process(#'ProtocolUnitUpdate'{milliseconds = Ms, units = PUnits}, #player{roompid = RoomPid} = State) ->
    Units = eatrun_utils:protocol_units_to_server_units(PUnits, Ms),
    gen_server:cast(RoomPid, {unitupdate, Units}),
    State;


process(#'ProtocolUnitRemove'{}, State) ->
    erlang:error("Not Implement: ProtocolUnitRemove"),
    State.

