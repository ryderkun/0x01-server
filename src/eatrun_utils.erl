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
    uuid/0,
    timestamp/0,
    timestamp_in_milliseconds/0,
    server_units_to_protocol_units/2,
    protocol_units_to_server_units/1,
    protocol_units_to_server_units/2,
    random_point/2,
    random_color/0]).


uuid() ->
    Node = atom_to_list(node()),
    uuid:to_string(uuid:uuid3(uuid:uuid4(), Node)).

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


protocol_units_to_server_units(PUnits) ->
    protocol_units_to_server_units(PUnits, timestamp_in_milliseconds()).

protocol_units_to_server_units(PUnits, Ms) ->
    lists:map(
        fun(U) ->
            #'ProtocolUnit'{
                id = Id,
                name = Name,
                size = Size,
                color = Color,
                pos = #'ProtocolVector2'{x = Px, y = Py},
                towards = #'ProtocolVector2'{x = Tx, y = Ty},
                status = Status
            } = U,

            #'unit'{
                id = Id,
                name = Name,
                size = Size,
                color = Color,
                pos = [Px, Py],
                towards = [Tx, Ty],
                update_at = Ms,
                status = Status
            }
        end,

        PUnits
    ).

server_units_to_protocol_units(ServerUnits, init) ->
    Fun = fun(
        #unit{
            id = Id,
            name = Name,
            size = Size,
            color = Color,
            pos = [Px, Py],
            towards = [Tx, Ty],
            status = Status
        }
    ) ->
        #'ProtocolUnit'{
            id = Id,
            name = Name,
            size = Size,
            color = Color,
            pos = #'ProtocolVector2'{x = Px, y = Py},
            towards = #'ProtocolVector2'{x = Tx, y = Ty},
            status = Status
        }
    end,

    lists:map(Fun, ServerUnits);


server_units_to_protocol_units(ServerUnits, sync) ->
    Fun = fun(
        #unit{
            id = Id,
            size = Size,
            pos = [Px, Py],
            towards = [Tx, Ty],
            status = Status
        }
    ) ->
        case Status of
            'Move' ->
                {
                    true,
                    #'ProtocolUnit'{
                        id = Id,
                        size = Size,
                        pos = #'ProtocolVector2'{x = Px, y = Py},
                        towards = #'ProtocolVector2'{x = Tx, y = Ty},
                        status = Status
                    }
                };
            _ ->
                false
        end
    end,

    lists:filtermap(Fun, ServerUnits).