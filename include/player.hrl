%%%-------------------------------------------------------------------
%%% @author wang <yueyoum@gmail.com>
%%% @copyright (C) 2015
%%% @doc
%%%
%%% @end
%%% Created : 2015-05-27 01:51
%%%-------------------------------------------------------------------
-author("wang").

-type unit_status()     :: 'Idle' | 'Move' | 'Jump'.

-export_type([unit_status/0]).

-record(player, {
    ids          :: gb_sets:set(),
    roompid      :: pid()
}).


-record(unit, {
    id           :: integer(),
    name         :: string(),
    score        :: float(),
    color        :: integer(),
    pos          :: {float(), float()},
    towards      :: {float(), float()},
    update_at    :: integer(),
    status       :: unit_status(),
    changed      :: boolean()
}).
