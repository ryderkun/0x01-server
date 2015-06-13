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
    {ok, Hz} = application:get_env(eatrun, sync_hz),
    Interval = 1000 div Hz,

    %% send Config message
    MsgConfig = #'ProtocolConfig'{sync_interval = Interval},
    MsgData = protocol_handler:pack_with_id(MsgConfig),
    self() ! {notify, MsgData},

    {cowboy_websocket, Req, #player{ids = gb_sets:new()}}.


websocket_handle({binary, Data}, Req, State) ->
    NewState = protocol_handler:process(Data, State),
    {ok, Req, NewState};


websocket_handle({text, _}, Req, State) ->
    {stop, Req, State}.


websocket_info(room_down, Req, State) ->
    {stop, Req, State#player{roompid = undefined}};


websocket_info({notify, Data}, Req, State) ->
    {reply, {binary, Data}, Req, State}.


terminate(_Reason, _Req, #player{ids = Ids, roompid = RoomPid}) ->
    io:format("ws_handler, terminate...~n"),
    case is_pid(RoomPid) of
        true ->
            gen_server:cast(RoomPid, {exit, self(), gb_sets:to_list(Ids)});
        false ->
            ok
    end,
    ok.

