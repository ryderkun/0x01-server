%%%-------------------------------------------------------------------
%%% @author wang <yueyoum@gmail.com>
%%% @copyright (C) 2015
%%% @doc
%%%
%%% @end
%%% Created : 2015-05-28 18:11
%%%-------------------------------------------------------------------
-module(eatrun_utils).
-author("wang").

%% API
-export([timestamp/0,
         timestamp_in_milliseconds/0]).

timestamp() ->
    {A, B, _C} = os:timestamp(),
    A * 1000000 + B.

timestamp_in_milliseconds() ->
    {A, B, C} = os:timestamp(),
    A * 1000000000 + B * 1000 + round(C/1000).
