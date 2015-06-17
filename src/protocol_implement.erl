
-module(protocol_implement).

-include("protocol.hrl").
-include("player.hrl").

-export([process/2]).

process(#'ProtocolTimeSync'{} = Msg, #player{roompid = undefined} = State) ->
    reply_time_sync(Msg),

    {ok, Room} = eatrun_room_server:find_room(),
    {ok, InitData} = gen_server:call(Room, {join, self()}),

    self() ! {notify, InitData},
    {ok, State#player{roompid = Room}};

process(#'ProtocolTimeSync'{} = Msg, #player{roompid = RoomPid} = State) when is_pid(RoomPid) ->
    reply_time_sync(Msg),
    {ok, State};

process(#'ProtocolUnitCreate'{}, #player{unit_id = UnitId}) when is_binary(UnitId) ->
    error;

process(#'ProtocolUnitCreate'{name = Name, pos = Pos}, #player{unit_id = undefined, roompid = RoomPid} = State) ->
    Id = eatrun_utils:make_id(?UNIT_ID_PREFIX),
    Color = eatrun_utils:random_color(),

    #'ProtocolVector2'{x = Px, y = Py} = Pos,

    {Size, Speed} = eatrun_utils:score_to_size_and_speed(?UNIT_INIT_SCORE),

    Unit = #'unit'{
        id = Id,
        name = Name,
        score = ?UNIT_INIT_SCORE,
        size = Size,
        color = Color,
        pos = {Px, Py},
        towards = {0.0, 0.0},
        speed = Speed,
        player_pid = self()
    },

    MsgUnitAdd = #'ProtocolUnitAdd'{is_own = true, units = eatrun_utils:server_units_to_protocol_units([Unit], init)},
    DataUnitAdd = protocol_handler:pack_with_id(MsgUnitAdd),
    self() ! {notify, DataUnitAdd},

    gen_server:cast(RoomPid, {unit_create, Unit}),
    {ok, State#player{unit_id = Id}};


process(#'ProtocolUnitMove'{target = Target}, #player{roompid = RoomPid, unit_id = UnitId} = State) ->
    gen_server:cast(RoomPid, {unit_move, UnitId, Target}),
    {ok, State};

process(#'ProtocolUnitSpeedUp'{}, #player{roompid = RoomPid, unit_id = UnitId} = State) ->
    gen_server:cast(RoomPid, {unit_speed_up, UnitId}),
    {ok, State};

process(#'ProtocolUnitSpeedNormal'{}, #player{roompid = RoomPid, unit_id = UnitId} = State) ->
    gen_server:cast(RoomPid, {unit_speed_normal, UnitId}),
    {ok, State}.

%% ==================


reply_time_sync(Msg) ->
    MsgTimeSync = Msg#'ProtocolTimeSync'{server = eatrun_utils:timestamp_in_milliseconds()},
    Data = protocol_handler:pack_with_id(MsgTimeSync),
    self() ! {notify, Data},
    ok.
