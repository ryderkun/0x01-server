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

-record(state, {interval        :: number(),                %% the interval milliseconds to sync
                start_speed     :: number(),                %% A unit's init speed
                units           :: #{},                     %% { id => #unit{} }
                players         :: [pid()],
                dots            :: #{},                     %% { id => #'ProtocolDot'{} }
                dots_add        :: #{},
                dots_remove     :: list()
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
    {ok, StartSpeed} = application:get_env(eatrun, start_speed),
    {ok, Hz} = application:get_env(eatrun, sync_hz),
    Interval = 1000 div Hz,
    erlang:start_timer(Interval, self(), sync),

    Dots = generate_random_dots(-80, -50, 80, 50, 50),
    {ok, #state{interval = Interval, start_speed = StartSpeed, units = maps:new(), players = [], dots = Dots, dots_add = maps:new(), dots_remove = []}}.

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
handle_cast({dotremove, Ids}, #state{dots = Dots, dots_add = DotsAdd, dots_remove = DotsRemoved} = State) ->
    io:format("DotRemove...~n"),
    NewRemoved = lists:append(Ids, DotsRemoved),

    NewAdd =
    case maps:size(Dots) of
        N when N =< 30 ->
            maps:merge(DotsAdd, generate_random_dots(-80, -50, 80, 50, 30));
        _ ->
            DotsAdd
    end,

    {noreply, State#state{dots_add = NewAdd, dots_remove = NewRemoved}};



handle_cast({unit_create, Unit, FromPid}, #state{units = Units, players = Players} = State) ->
    io:format("unit_create...~n"),
    NewUnits = maps:put(Unit#unit.id, Unit, Units),

    MsgUnitAdd = #'ProtocolUnitAdd'{is_own = false, units = eatrun_utils:server_units_to_protocol_units([Unit], init)},
    DataUnitAdd = protocol_handler:pack_with_id(MsgUnitAdd),

    lists:foreach(
        fun(P) -> P ! {notify, DataUnitAdd} end,
        lists:delete(FromPid, Players)
    ),

    {noreply, State#state{units = NewUnits}};


handle_cast({unit_move, UnitMoves, UpdateAt}, #state{units = Units} = State) ->
    NewUnits = lists:foldl(
        fun(U, Acc) ->
            #'ProtocolUnitMove.UnitMoving'{
                id = Id,
                pos = #'ProtocolVector2'{x = Px, y = Py},
                direction = #'ProtocolVector2'{x = Dx, y = Dy}
            } = U,

            case maps:find(Id, Acc) of
                {ok, Value} ->
                    NewValue = Value#unit{
                        %pos = {Px, Py},
                        towards = {Dx, Dy},
                        update_at = UpdateAt
                    },

                    maps:update(Id, NewValue, Acc);
                error ->
                    Acc
            end
        end,
        Units,
        UnitMoves
    ),

    {noreply, State#state{units = NewUnits}};


handle_cast({exit, PlayerPid, UnitIds}, #state{units = Units, players = Players} = State) ->
    io:format("Room Instance: ~p exit with unit ids ~p~n", [PlayerPid, UnitIds]),

    NewUnits = maps:without(UnitIds, Units),
    NewPlayers = lists:delete(PlayerPid, Players),
    {noreply, State#state{units = NewUnits, players = NewPlayers}}.


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
handle_info({timeout, _TimerRef, sync}, #state{interval = Interval, units = Units, players = Players, dots = Dots, dots_add = DotAdd, dots_remove = DotRemove} = State) ->

    %% Send the whole scene to all players
    NewDots1 = maps:without(DotRemove, Dots),
    NewDots2 = maps:merge(NewDots1, DotAdd),

    Now = eatrun_utils:timestamp_in_milliseconds(),


    NewUnits = maps:map(
        fun(_Id, V) ->
            #unit{
                pos = {Px, Py},
                towards = {Dx, Dy},
                speed = Speed,
                update_at = UpdateAt
            } = V,

            io:format("Px = ~p, Now - ~p, UpdateAt = ~p, Speed - ~p, Dx = ~p~n", [
                Px, Now, UpdateAt, Speed, Dx
            ]),
            NewPosX = Px + Interval * Speed * Dx / 1000,
            NewPosY = Py + Interval * Speed * Dy / 1000,

            V#unit{
                pos = {NewPosX, NewPosY},
                update_at = Now
            }
        end,

        Units
    ),


    MsgSceneSync = #'ProtocolSceneSync'{
        update_at = Now,
        unit_updates = eatrun_utils:server_units_to_protocol_units(maps:values(NewUnits), sync),
        dot_adds = maps:values(DotAdd),
        dot_removes = DotRemove
    },

    DataSceneSync = protocol_handler:pack_with_id(MsgSceneSync),

    lists:foreach(
        fun(P) -> P ! {notify, DataSceneSync} end,
        Players
    ),

    erlang:start_timer(Interval, self(), sync),
    {noreply, State#state{units = NewUnits, dots = NewDots2, dots_add = maps:new(), dots_remove = []}}.

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


generate_random_dots(MinX, MinY, MaxX, MaxY, Amount) ->
    io:format("generate_random_dots~n"),

    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed(A, B, C),

    lists:foldl(
        fun(_, Acc) ->
            Id = eatrun_utils:uuid(),
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
