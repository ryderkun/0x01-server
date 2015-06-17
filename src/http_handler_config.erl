%%%-------------------------------------------------------------------
%%% @author wang <yueyoum@gmail.com>
%%% @copyright (C) 2015
%%% @doc
%%%
%%% @end
%%% Created : 2015-05-27 23:05
%%%-------------------------------------------------------------------
-module(http_handler_config).
-author("wang").

%% API
-export([init/2]).

-include("protocol.hrl").

init(Req, Opts) ->
    {ok, Hz} = application:get_env(eatrun, sync_hz),
    {ok, {MapX, MapY}} = application:get_env(eatrun, map_bounds),

    MsgConfig = #'ProtocolConfig'{
        sync_interval = 1000 div Hz,
        map_x = MapX,
        map_y = MapY
    },

    MsgData = protocol_handler:pack_with_id(MsgConfig),
    Req2 = cowboy_req:reply(200, [], MsgData, Req),
    {ok, Req2, Opts}.
