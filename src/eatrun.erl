%%%-------------------------------------------------------------------
%%% @author wang <yueyoum@gmail.com>
%%% @copyright (C) 2015
%%% @doc
%%%
%%% @end
%%% Created : 2015-05-27 23:08
%%%-------------------------------------------------------------------
-module(eatrun).
-author("wang").

%% API
-export([start/0]).

start() ->
    application:start(crypto),
    application:start(cowlib),
    application:start(ranch),
    application:start(cowboy),
    application:start(eatrun),
    ok.
