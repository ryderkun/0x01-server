%%%-------------------------------------------------------------------
%%% @author wang <yueyoum@gmail.com>
%%% @copyright (C) 2015
%%% @doc
%%%
%%% @end
%%% Created : 2015-05-27 01:51
%%%-------------------------------------------------------------------
-author("wang").


-define(UNIT_ID_PREFIX, "u").
-define(DOT_ID_PREFIX, "d").
-define(DOT_SCORE, 2).

-record(player, {
    ids          :: list(),
    roompid      :: pid()
}).


-record(unit, {
    id           :: string(),
    name         :: string(),
    score        :: integer(),
    size         :: float(),
    color        :: integer(),
    pos          :: {float(), float()},
    towards      :: {float(), float()},
    speed        :: float()
}).
