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

-include("protocol.hrl").
-include("player.hrl").

%% API
-export([
    make_id/1,
    get_id_prefix/1,
    timestamp/0,
    timestamp_in_milliseconds/0,
    server_units_to_protocol_units/2,
    random_point/2,
    random_color/0,
    score_to_size/1,
    score_to_speed/1]).


uuid() ->
    Node = atom_to_list(node()),
    UUID = uuid:to_string(uuid:uuid3(uuid:uuid4(), Node)),
    list_to_binary(UUID).

make_id(Prefix) ->
    UUID = uuid(),
    <<Prefix, UUID/binary>>.

get_id_prefix(Id) ->
    PrefixInt = binary:first(Id),
    [PrefixStr] = io_lib:fwrite("~c", [PrefixInt]),
    PrefixStr.


timestamp() ->
    {A, B, _C} = os:timestamp(),
    A * 1000000 + B.

timestamp_in_milliseconds() ->
    {A, B, C} = os:timestamp(),
    A * 1000000000 + B * 1000 + round(C/1000).


random_point(Min, Max) ->
    Diff = Max - Min,
    Factor = random:uniform(),
    Min + Factor * Diff.

random_color() ->
    R = random:uniform(100) + 150,
    G = random:uniform(100) + 150,
    B = random:uniform(100) + 150,
    <<Color:32>> = <<0:8, R:8, G:8, B:8>>,
    Color.

server_units_to_protocol_units(ServerUnits, init) ->
    Fun = fun(
        #unit{
            id = Id,
            name = Name,
            size = Size,
            color = Color,
            pos = {Px, Py}
        }
    ) ->
        #'ProtocolUnit'{
            id = Id,
            name = Name,
            size = Size,
            color = Color,
            pos = #'ProtocolVector2'{x = Px, y = Py}
        }
    end,

    lists:map(Fun, ServerUnits);


server_units_to_protocol_units(ServerUnits, sync) ->
    Fun = fun(
        #unit{
            id = Id,
            size = Size,
            pos = {Px, Py}
        }
    ) ->
        #'ProtocolUnit'{
            id = Id,
            pos = #'ProtocolVector2'{x = Px, y = Py},
            size = Size
        }
    end,

    lists:map(Fun, ServerUnits).

score_to_size(Score) ->
    5.

score_to_speed(Score) ->
    20.
