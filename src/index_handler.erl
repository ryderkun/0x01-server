%%%-------------------------------------------------------------------
%%% @author wang <yueyoum@gmail.com>
%%% @copyright (C) 2015
%%% @doc
%%%
%%% @end
%%% Created : 2015-05-27 23:05
%%%-------------------------------------------------------------------
-module(index_handler).
-author("wang").

%% API
-export([init/2]).

init(Req, Opts) ->
    Req2 = cowboy_req:reply(200, [], <<"Hello">>, Req),
    {ok, Req2, Opts}.
