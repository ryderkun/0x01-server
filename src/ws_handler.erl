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

-include("player.hrl").

init(Req, _Opts) ->
    io:format("WSBSOCKET INIT~n"),
    {ok, RoomPid} = eatrun_room_server:join_room(self()),
    MonitorRef = erlang:monitor(process, RoomPid),
    {cowboy_websocket, Req, #player{ids = gb_sets:new(), roompid = RoomPid, monitorref = MonitorRef}}.

websocket_handle({binary, Data}, Req, State) ->
    NewState = protocol_handler:process(Data, State),
    {ok, Req, NewState}.

websocket_info({'DOWN', MonitorRef, _, _, _}, Req, #player{monitorref = MonitorRef} = State) ->
    io:format("Room Down!!! ~n"),
    {stop, Req, State};

websocket_info({notify, Data}, Req, State) ->
    io:format("ws got notify~n"),
    {reply, {binary, Data}, Req, State}.

terminate(_Reason, _Req, #player{ids = Ids, roompid = RoomPid}) ->
    io:format("terminate...~n"),

    gen_server:cast(RoomPid, {exit, self(), gb_sets:to_list(Ids)}),
    ok.


