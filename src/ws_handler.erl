%%%-------------------------------------------------------------------
%%% @author wang <yueyoum@gmail.com>
%%% @copyright (C) 2015
%%% @doc
%%%
%%% @end
%%% Created : 2015-05-26 22:59
%%%-------------------------------------------------------------------
-module(ws_handler).
-author("wang").

%% API
-export([init/2,
         websocket_handle/3,
         websocket_info/3,
         terminate/3]).

-include("protocol.hrl").
-include("player.hrl").

init(Req, _Opts) ->
    io:format("WSBSOCKET INIT~n"),


    {cowboy_websocket, Req, #player{}}.


websocket_handle({binary, Data}, Req, State) ->
    case protocol_handler:process(Data, State) of
        {ok, NewState} -> {ok, Req, NewState};
        error -> {stop, Req, State}
    end;


websocket_handle({text, _}, Req, State) ->
    {stop, Req, State}.

websocket_info(unit_eaten, Req, State) ->
    io:format("unit_eaten...~n"),
    {ok, Req, State#player{unit_id = undefined}};

websocket_info(room_down, Req, State) ->
    {stop, Req, State#player{roompid = undefined}};

websocket_info({notify, Data}, Req, State) ->
    {reply, {binary, Data}, Req, State}.

terminate(_Reason, _Req, #player{unit_id = UnitId, roompid = RoomPid}) when UnitId == undefined; RoomPid == undefined ->
    ok;

terminate(_Reason, _Req, #player{unit_id = UnitId, roompid = RoomPid}) ->
    io:format("ws_handler, terminate...~n"),
    case is_pid(RoomPid) of
        true ->
            gen_server:cast(RoomPid, {exit, self(), UnitId});
        false ->
            ok
    end,
    ok.

