%%%-------------------------------------------------------------------
%%% @author wang <yueyoum@gmail.com>
%%% @copyright (C) 2015
%%% @doc
%%%
%%% @end
%%% Created : 2015-05-26 21:55
%%%-------------------------------------------------------------------
-module(eatrun_room_instance).
-author("wang").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-include("protocol.hrl").
-include("player.hrl").

-record(state, {map_size        :: {integer(), integer(), integer(), integer()},
                interval        :: integer(),                %% the interval milliseconds to sync
                players         :: [pid()],
                units           :: #{},                      %% { id => #unit{} }
                units_remove    :: [string()],
                dots            :: #{},                      %% { id => #'ProtocolDot'{} }
                dots_add        :: #{},
                quadmap
}).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed(A, B, C),

    {ok, Hz} = application:get_env(eatrun, sync_hz),
    {ok, {MapX, MapY}} = application:get_env(eatrun, map_bounds),
    {ok, MapSplits} = application:get_env(eatrun, map_splits),
    {ok, DotAmount} = application:get_env(eatrun, dot_amount),

    Interval = 1000 div Hz,
    MapSize = {-MapX, -MapY, MapX, MapY},

    Dots = generate_random_dots(MapSize, DotAmount),
    QuadMap = quadmaps:new(MapSize, MapSplits),
    QuadMap1 =
    lists:foldl(
        fun(#'ProtocolDot'{id = Did, pos = #'ProtocolVector2'{x = Dx, y = Dy}}, Acc) ->
            quadmaps:put(Acc, Did, Dx, Dy)
        end,
        QuadMap,
        maps:values(Dots)
    ),

    State = #state{
        map_size = MapSize,
        interval = Interval,
        players = [],
        units = maps:new(),
        units_remove = [],
        dots = Dots,
        dots_add = maps:new(),
        quadmap = QuadMap1
    },


    erlang:start_timer(Interval, self(), sync),

    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call({join, PlayerPid}, _From, #state{units = Units, players = Players, dots = Dots} = State) ->
    io:format("Room Instance: ~p join~n", [PlayerPid]),

    MsgSceneInit = #'ProtocolSceneInit'{
        unit_adds = eatrun_utils:server_units_to_protocol_units(maps:values(Units), init),
        dot_adds = maps:values(Dots)
    },

    DataSceneInit = protocol_handler:pack_with_id(MsgSceneInit),
    {reply, {ok, DataSceneInit}, State#state{players = [PlayerPid | Players]}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({unit_create, Unit}, #state{units = Units, players = Players, quadmap = QuadMap} = State) ->
    io:format("unit_create...~n"),
    #unit{id = Id, pos = {Px, Py}, player_pid = PlayerPid} = Unit,
    NewUnits = maps:put(Id, Unit, Units),
    NewQuadMap = quadmaps:put(QuadMap, Id, Px, Py),

    MsgUnitAdd = #'ProtocolUnitAdd'{is_own = false, units = eatrun_utils:server_units_to_protocol_units([Unit], init)},
    DataUnitAdd = protocol_handler:pack_with_id(MsgUnitAdd),

    lists:foreach(
        fun(P) -> P ! {notify, DataUnitAdd} end,
        lists:delete(PlayerPid, Players)
    ),

    {noreply, State#state{units = NewUnits, quadmap = NewQuadMap}};


handle_cast({unit_move, Id, #'ProtocolVector2'{x = Tx, y = Ty}}, #state{units = Units} = State) ->
    Unit = maps:get(Id, Units),
    NewUnits = maps:update(Id, Unit#unit{towards = {Tx, Ty}}, Units),

    {noreply, State#state{units = NewUnits}};

handle_cast({unit_speed_up, Id}, #state{units = Units} = State) ->
    Unit = maps:get(Id, Units),
    NewUnits = maps:update(Id, Unit#unit{status = speedup}, Units),

    {noreply, State#state{units = NewUnits}};

handle_cast({unit_speed_normal, Id}, #state{units = Units} = State) ->
    Unit = maps:get(Id, Units),
    NewUnits = maps:update(Id, Unit#unit{status = normal}, Units),

    {noreply, State#state{units = NewUnits}};


handle_cast({exit, PlayerPid, UnitId}, #state{
    units = Units,
    units_remove = UnitsRemove,
    players = Players,
    quadmap = QuadMap} = State) ->

    io:format("Room Instance: ~p exit with unit ids ~p~n", [PlayerPid, UnitId]),

    NewUnits = maps:without([UnitId], Units),
    NewPlayers = lists:delete(PlayerPid, Players),
    NewQuadMap = quadmaps:delete(QuadMap, UnitId),

    NewState = State#state{
        players = NewPlayers,
        units = NewUnits,
        units_remove = [UnitId | UnitsRemove],
        quadmap = NewQuadMap
    },

    {noreply, NewState}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info({timeout, _TimerRef, sync}, #state{
    interval = Interval,
    players = Players,
    units = Units,
    units_remove = UnitsRemove,
    dots = Dots,
    dots_add = DotAdd,
    quadmap = QuadMap} = State) ->


    Now = eatrun_utils:timestamp_in_milliseconds(),


    %% detect eat
    {NewUnits1, NewDots, NewQuadMap, UnitEated, DotEated} = detect_eat(Units, Dots, QuadMap),

    %% move
    NewUnits2 = maps:map(
        fun(_Id, V) -> update_unit(V#unit.status, Interval, V) end,
        NewUnits1
    ),


    %% Send the whole scene to all players
    MsgSceneSync = #'ProtocolSceneSync'{
        update_at = Now,
        unit_updates = eatrun_utils:server_units_to_protocol_units(maps:values(NewUnits2), sync),
        unit_removes = UnitsRemove ++ UnitEated,
        dot_adds = maps:values(DotAdd),
        dot_removes = DotEated
    },

    DataSceneSync = protocol_handler:pack_with_id(MsgSceneSync),
    lists:foreach(
        fun(P) -> P ! {notify, DataSceneSync} end,
        Players
    ),

    %% Send unit_eaten to related players
    lists:foreach(
        fun(Uid) ->
            #unit{player_pid = PlayerId} = maps:get(Uid, Units),
            PlayerId ! unit_eaten
        end,
        UnitEated
    ),

    NewState = State#state{
        units = NewUnits2,
        units_remove = [],
        dots = NewDots,
        dots_add = maps:new(),
        quadmap = NewQuadMap
    },

    erlang:start_timer(Interval, self(), sync),
    {noreply, NewState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, #state{players = Players}) ->
    io:format("Room Down...~n"),

    lists:foreach(
        fun(P) -> P ! room_down end,
        Players
    ),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


generate_random_dots({MinX, MinY, MaxX, MaxY}, Amount) ->
    io:format("generate_random_dots~n"),
    lists:foldl(
        fun(_, Acc) ->
            Id = eatrun_utils:make_id(?DOT_ID_PREFIX),
            Px = eatrun_utils:random_point(MinX, MaxX),
            Py = eatrun_utils:random_point(MinY, MaxY),
            Color = eatrun_utils:random_color(),
            Dot = #'ProtocolDot'{
                id = Id,
                pos = #'ProtocolVector2'{x = Px, y = Py},
                color = Color
            },

            maps:put(Id, Dot, Acc)
        end,
        maps:new(),
        lists:seq(1, Amount)
    ).


%% towards(Px, Py, Tx, Ty) ->
%%     XDiff = Tx - Px,
%%     YDiff = Ty - Py,
%%     Length = math:sqrt(math:pow(XDiff, 2) + math:pow(YDiff, 2)),
%%     {XDiff / Length, YDiff / Length}.


detect_eat(Units, Dots, QuadMap) ->
    UnitsList = lists:sort(
        fun(#unit{size = A}, #unit{size = B}) -> A > B end,
        maps:values(Units)
    ),

    do_detect_eat(UnitsList, Units, Dots, QuadMap, [], []).

do_detect_eat([], Units, Dots, QuadMap, UnitBeenEaten, DotsBeenEaten) ->
    {Units, Dots, QuadMap, UnitBeenEaten, DotsBeenEaten};

do_detect_eat([U | Rest], Units, Dots, QuadMap, UnitBeenEaten, DotsBeenEaten) ->
    #unit{id = Id, score = Score, size = Size, pos = {Px, Py}} = U,
    case maps:is_key(Id, Units) of
        false ->
            do_detect_eat(Rest, Units, Dots, QuadMap, UnitBeenEaten, DotsBeenEaten);
        true ->
            Bounds = {Px - Size, Py - Size, Px + Size, Py + Size},

            DetectedIds = quadmaps:find(QuadMap, Bounds),

            DetectedDotIds = lists:filter(
                fun(DetectId) ->
                    case eatrun_utils:get_id_prefix(DetectId) of
                        ?DOT_ID_PREFIX ->
                            #'ProtocolDot'{pos = #'ProtocolVector2'{x = X, y = Y}} = maps:get(DetectId, Dots),
                            is_in_unit_scope(Px, Py, Size, X, Y);
                        _ ->
                            false
                    end
                end,
                DetectedIds
            ),
            NewDots = maps:without(DetectedDotIds, Dots),

            NewDotsBeenEaten = DotsBeenEaten ++ DetectedDotIds,

            io:format("~p touch dots: ~p~n", [Id, DetectedDotIds]),

            DetectedUnitIds = lists:filter(
                fun(DetectId) ->
                    case Id == DetectId of
                        true ->
                            false;
                        false ->
                            case eatrun_utils:get_id_prefix(DetectId) of
                                ?UNIT_ID_PREFIX ->
                                    #unit{pos = {X, Y}} = maps:get(DetectId, Units),
                                    is_in_unit_scope(Px, Py, Size * 0.9, X, Y);
                                _ ->
                                    false
                            end
                    end
                end,
                DetectedIds
            ),

            io:format("~p touch units: ~p~n", [Id, DetectedUnitIds]),

            NewUnitBeenEaten = UnitBeenEaten ++ DetectedUnitIds,

            NewScore =
                lists:foldl(
                    fun(DUId, Acc) ->
                        #unit{size = S} = maps:get(DUId, Units),
                        Acc + S * 0.9
                    end,
                    Score + length(DetectedDotIds) * ?DOT_SCORE,
                    DetectedUnitIds
                ),
            {NewSize, NewSpeed} = eatrun_utils:score_to_size_and_speed(NewScore),

            NewUnits1 = maps:without(DetectedUnitIds, Units),

            NewU = U#unit{
                score = NewScore,
                size = NewSize,
                speed = NewSpeed
            },

            NewUnits2 = maps:update(Id, NewU, NewUnits1),


            NewQuadMap =
                lists:foldl(
                    fun(Did, Acc) -> quadmaps:delete(Acc, Did) end,
                    QuadMap,
                    DetectedDotIds ++ DetectedUnitIds
                ),

            do_detect_eat(Rest, NewUnits2, NewDots, NewQuadMap, NewUnitBeenEaten, NewDotsBeenEaten)
    end.



is_in_unit_scope(Px, Py, Size, TargetX, TargetY) ->
    math:pow(TargetX-Px, 2) + math:pow(TargetY-Py, 2) =< math:pow(Size/2, 2).


update_unit(normal, Interval, #unit{
    pos = {Px, Py},
    towards = {Tx, Ty},
    speed = Speed} = Unit) ->

    NewPosX = Px + Interval * Speed * Tx / 1000,
    NewPosY = Py + Interval * Speed * Ty / 1000,
    Unit#unit{pos = {NewPosX, NewPosY}};

update_unit(speedup, Interval, #unit{score = Score} = Unit) ->
    {NewScore, NewStatus} =
    case Score * 0.9 of
        N when N < ?UNIT_INIT_SCORE ->
            % auto turn to normal
            {?UNIT_INIT_SCORE, normal};
        N ->
            {N, speedup}
    end,

    {NewSize, NewSpeed} = eatrun_utils:score_to_size_and_speed(NewScore),
    NewSpeed1 =
    case NewStatus of
        normal -> NewSpeed;
        speedup -> NewSpeed * ?UNIT_SPEED_UP_MULTI
    end,

    NewUnit = Unit#unit{
    score = NewScore,
    size = NewSize,
    speed = NewSpeed1,
    status = NewStatus
    },

    update_unit(normal, Interval, NewUnit).
