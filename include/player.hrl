%%%-------------------------------------------------------------------
%%% @author wang <yueyoum@gmail.com>
%%% @copyright (C) 2015
%%% @doc
%%%
%%% @end
%%% Created : 2015-05-27 01:51
%%%-------------------------------------------------------------------
-author("wang").


-record(player, {
    ids          :: gb_sets:set(),
    roompid      :: pid()
}).


-record(unit, {
    id           :: integer(),
    name         :: string(),
    score        :: integer(),
    size         :: float(),
    color        :: integer(),
    pos          :: {float(), float()},
    towards      :: {float(), float()},
    speed        :: float(),
    update_at    :: integer()
}).
