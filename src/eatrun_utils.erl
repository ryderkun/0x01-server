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
    timestamp/0,
    timestamp_in_milliseconds/0,
    server_units_to_protocol_units/1,
    server_units_to_protocol_units/2,
    protocol_units_to_server_units/1,
    protocol_units_to_server_units/2
]).


timestamp() ->
    {A, B, _C} = os:timestamp(),
    A * 1000000 + B.

timestamp_in_milliseconds() ->
    {A, B, C} = os:timestamp(),
    A * 1000000000 + B * 1000 + round(C/1000).


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
                towards = #'ProtocolVector2'{x = Tx, y = Ty}
            } = U,

            #'unit'{
                id = Id,
                name = Name,
                size = Size,
                color = Color,
                pos = [Px, Py],
                towards = [Tx, Ty],
                milliseconds = Ms
            }
        end,

        PUnits
    ).

server_units_to_protocol_units(SUnits) ->
    server_units_to_protocol_units(SUnits, all).

server_units_to_protocol_units(SUnits, Method) ->
    lists:map(
        fun(U) ->
            #unit{
                id = Id,
                name = Name,
                size = Size,
                color = Color,
                pos = [Px, Py],
                towards = [Tx, Ty]
            } = U,

            case Method of
                all ->
                    #'ProtocolUnit'{
                        id = Id,
                        name = Name,
                        size = Size,
                        color = Color,
                        pos = #'ProtocolVector2'{x = Px, y = Py},
                        towards = #'ProtocolVector2'{x = Tx, y = Ty}
                    };
                update ->
                    #'ProtocolUnit'{
                        id = Id,
                        size = Size,
                        pos = #'ProtocolVector2'{x = Px, y = Py},
                        towards = #'ProtocolVector2'{x = Tx, y = Ty}
                    }
            end
        end,

        SUnits
    ).
