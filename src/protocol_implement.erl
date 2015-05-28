
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

    Units = convert_punits_to_units(PUnits),
    gen_server:cast(RoomPid, {unitadd, Units, Msg, self()}),
    State#player{ids = NewIds};


process(#'ProtocolUnitUpdate'{milliseconds = Ms, units = PUnits} = Msg, #player{roompid = RoomPid} = State) ->
    Now = eatrun_utils:timestamp_in_milliseconds(),
    io:format("Server - Client = ~p~n", [Now - Ms]),
    Units = convert_punits_to_units(PUnits, Ms),
    gen_server:cast(RoomPid, {unitupdate, Units, Msg, self()}),
    State;


process(#'ProtocolUnitRemove'{}, State) ->
    erlang:error("Not Implement: ProtocolUnitRemove"),
    State.


convert_punits_to_units(PUnits) ->
    convert_punits_to_units(PUnits, eatrun_utils:timestamp_in_milliseconds()).

convert_punits_to_units(PUnits, Ms) ->
    lists:map(
        fun(U) ->
            #'unit'{
                id = U#'ProtocolUnit'.id,
                name = U#'ProtocolUnit'.name,
                size = U#'ProtocolUnit'.size,
                color = U#'ProtocolUnit'.color,
                pos = U#'ProtocolUnit'.pos,
                move_vector = U#'ProtocolUnit'.move_vector,
                milliseconds = Ms
            }
        end,

        PUnits
    ).
