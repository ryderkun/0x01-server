
-module(protocol_implement).

-include("protocol.hrl").
-include("player.hrl").

-export([process/2]).

process(#'ProtocolTimeSync'{} = Msg, #player{roompid = undefined} = State) ->
    reply_time_sync(Msg),

    {ok, Room} = eatrun_room_server:find_room(),
    {ok, InitData} = gen_server:call(Room, {join, self()}),

    self() ! {notify, InitData},
    State#player{roompid = Room};

process(#'ProtocolTimeSync'{} = Msg, #player{roompid = RoomPid} = State) when is_pid(RoomPid) ->
    reply_time_sync(Msg),
    State;


process(#'ProtocolUnitCreate'{name = Name, pos = Pos}, #player{ids = Ids, roompid = RoomPid} = State) ->
    Id = eatrun_utils:make_id(?UNIT_ID_PREFIX),
    Color = eatrun_utils:random_color(),

    #'ProtocolVector2'{x = Px, y = Py} = Pos,

    Unit = #'unit'{
        id = Id,
        name = Name,
        score = 0,
        size = eatrun_utils:score_to_size(0),
        color = Color,
        pos = {Px, Py},
        towards = {0.0, 0.0},
        speed = eatrun_utils:score_to_speed(0)
    },

    MsgUnitAdd = #'ProtocolUnitAdd'{is_own = true, units = eatrun_utils:server_units_to_protocol_units([Unit], init)},
    DataUnitAdd = protocol_handler:pack_with_id(MsgUnitAdd),
    self() ! {notify, DataUnitAdd},

    gen_server:cast(RoomPid, {unit_create, Unit, self()}),
    State#player{ids = [Id | Ids]};


process(#'ProtocolUnitMove'{target = Target}, #player{roompid = RoomPid, ids = Ids} = State) ->
    gen_server:cast(RoomPid, {unit_move, Ids, Target}),
    State;

process(#'ProtocolUnitSplit'{}, #player{roompid = RoomPid} = State) ->
    gen_server:cast(RoomPid, unit_split),
    State;

process(#'ProtocolUnitEject'{}, #player{roompid = RoomPid} = State) ->
    gen_server:cast(RoomPid, unit_eject),
    State.

%% ==================


reply_time_sync(Msg) ->
    MsgTimeSync = Msg#'ProtocolTimeSync'{server = eatrun_utils:timestamp_in_milliseconds()},
    Data = protocol_handler:pack_with_id(MsgTimeSync),
    self() ! {notify, Data},
    ok.
