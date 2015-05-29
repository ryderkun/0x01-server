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



-record(state, {
    units       :: #{},
    players     :: [pid()]
}).



-define(TIMER, 200).   % 1/10 s
-define(SPEED, 10).

-include("protocol.hrl").
-include("player.hrl").

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
    erlang:start_timer(?TIMER, self(), sync),
    {ok, #state{units = maps:new(), players = []}}.

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
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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
handle_cast({join, PlayerPid}, #state{units = Units, players = Players} = State) ->

    io:format("Room Instance: ~p join~n", [PlayerPid]),

    ProtocolUnits = eatrun_utils:server_units_to_protocol_units(maps:values(Units)),
    Msg = #'ProtocolUnitAdd'{units = ProtocolUnits},
    Data = protocol_handler:pack_with_id(Msg),

    PlayerPid ! {notify, Data},

    {noreply, State#state{players = [PlayerPid | Players]}};


handle_cast({unitadd, Us, Msg, FromPid}, #state{units = Units, players = Players} = State) ->
    UsMap = maps:from_list([{U#unit.id, U} || U <- Us]),
    NewUnits = maps:merge(Units, UsMap),

    Data = protocol_handler:pack_with_id(Msg),

    lists:foreach(
        fun(P) -> P ! {notify, Data} end,
        lists:delete(FromPid, Players)
    ),

    {noreply, State#state{units = NewUnits}};


handle_cast({unitupdate, ServerUtils}, #state{units = Units} = State) ->
    io:format("unitupdate...~n"),
    NewUnits = lists:foldl(
        fun(U, Acc) ->
            case maps:find(U#unit.id, Acc) of
                {ok, Value} ->
                    NewValue = Value#unit{
                        pos = U#unit.pos,
                        towards = U#unit.towards,
                        size = U#unit.size,
                        milliseconds = U#unit.milliseconds
                    },

                    maps:update(U#unit.id, NewValue, Acc);
                error ->
                    Acc
            end
        end,
        Units,
        ServerUtils
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
handle_info({timeout, _TimerRef, sync}, #state{units = Units, players = Players} = State) ->

    %% Send the whole scene to all players

    Now = eatrun_utils:timestamp_in_milliseconds(),

    NewUnits = maps:map(
        fun(_K, V) ->
            [MoveX, MoveY] = V#unit.towards,
            GoX = MoveX * ?SPEED * (Now - V#unit.milliseconds) / 1000,
            GoY = MoveY * ?SPEED * (Now - V#unit.milliseconds) / 1000,

            [OldX, OldY] = V#unit.pos,
            NewPos = [OldX + GoX, OldY + GoY],

            V#unit{pos = NewPos, milliseconds = Now}
        end,
        Units
    ),

    io:format("SYNC~n"),
    io:format("~p~n", [NewUnits]),

    ProtocolUtils = eatrun_utils:server_units_to_protocol_units(maps:values(NewUnits), update),
    Msg = #'ProtocolUnitUpdate'{
        milliseconds = Now,
        units = ProtocolUtils
    },

    Data = protocol_handler:pack_with_id(Msg),

    lists:foreach(fun(P) -> P ! {notify, Data} end, Players),
    erlang:start_timer(?TIMER, self(), sync),
    {noreply, State#state{units = NewUnits}}.

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
terminate(_Reason, _State) ->
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
