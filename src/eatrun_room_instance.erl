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

    MsgUnitAdd = #'ProtocolUnitAdd'{units = eatrun_utils:server_units_to_protocol_units(maps:values(Units), init)},
    MsgDotAdd = #'ProtocolDotAdd'{dots = maps:values(Dots)},
    MsgSceneInit = #'ProtocolSceneInit'{
        unit_adds = MsgUnitAdd,
        dot_adds = MsgDotAdd
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
    NewRemoved = lists:append(Ids, DotsRemoved),

    NewAdd =
    case maps:size(Dots) of
        N when N =< 30 ->
            maps:merge(DotsAdd, generate_random_dots(-80, -50, 80, 50, 30));
        _ ->
            DotsAdd
    end,

    {noreply, State#state{dots_add = NewAdd, dots_remove = NewRemoved}};



handle_cast({unitadd, ServerUnitsMaps, Data, FromPid}, #state{units = Units, players = Players} = State) ->
    NewUnits = maps:merge(Units, ServerUnitsMaps),

    lists:foreach(
        fun(P) -> P ! {notify, Data} end,
        lists:delete(FromPid, Players)
    ),

    {noreply, State#state{units = NewUnits}};


handle_cast({unitupdate, ProtocolUnits, Milliseconds}, #state{units = Units} = State) ->
    io:format("unitupdate...~n"),
    NewUnits = lists:foldl(
        fun(U, Acc) ->
            case maps:find(U#'ProtocolUnit'.id, Acc) of
                {ok, Value} ->
                    #'ProtocolUnit'{
                        pos = #'ProtocolVector2'{x = Px, y = Py},
                        towards = #'ProtocolVector2'{x = Tx, y = Ty},
                        status = Status,
                        score = Score
                    } = U,

                    NewValue = Value#unit{
                        score = Score,
                        pos = {Px, Py},
                        towards = {Tx, Ty},
                        update_at = Milliseconds,
                        status = Status,
                        changed = true
                    },

                    maps:update(U#'ProtocolUnit'.id, NewValue, Acc);
                error ->
                    Acc
            end
        end,
        Units,
        ProtocolUnits
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

    Now = eatrun_utils:timestamp_in_milliseconds(),

    NewDots1 = maps:without(DotRemove, Dots),
    NewDots2 = maps:merge(NewDots1, DotAdd),


    ProtocolUtils = eatrun_utils:server_units_to_protocol_units(maps:values(Units), sync),
    MsgUnitUpdate = #'ProtocolUnitUpdate'{
        milliseconds = Now,
        units = ProtocolUtils
    },
    MsgDotAdd = #'ProtocolDotAdd'{dots = maps:values(DotAdd)},
    MsgDotremove = #'ProtocolDotRemove'{ids = DotRemove},

    MsgSceneSync = #'ProtocolSceneSync'{
        unit_updates = MsgUnitUpdate,
        dot_adds = MsgDotAdd,
        dot_removes = MsgDotremove
    },

    DataSceneSync = protocol_handler:pack_with_id(MsgSceneSync),

    lists:foreach(
        fun(P) -> P ! {notify, DataSceneSync} end,
        Players
    ),

    NewUnits = maps:map(
        fun(_K, V) -> V#unit{changed = false} end,
        Units
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
