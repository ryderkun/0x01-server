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
-define(DOT_SCORE, 20).
-define(UNIT_INIT_SCORE, 5).
-define(UNIT_SPEED_UP_MULTI, 2).

-type status()   :: normal | speedup.
-export_type([status/0]).


-record(player, {
    unit_id             :: binary(),
    roompid             :: pid()
}).

-record(unit, {
    id                  :: binary(),
    name                :: string(),
    score               :: integer(),
    size                :: float(),
    color               :: integer(),
    pos                 :: {float(), float()},
    towards             :: {float(), float()},
    speed               :: float(),
    player_pid          :: pid(),
    status = normal     :: status()
}).
