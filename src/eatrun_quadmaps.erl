%%%-------------------------------------------------------------------
%%% @author wang <yueyoum@gmail.com>
%%% @copyright (C) 2015
%%% @doc
%%%
%%% @end
%%% Created : 2015-06-14 00:11
%%%-------------------------------------------------------------------
-module(eatrun_quadmaps).
-author("wang").

%% API
-export([new/5]).

-record(quad, {
    id          :: integer(),
    bounds      :: {float(), float(), float(), float()},            % {MinX, MinY, MaxX, MaxY}
    quardrants  :: {integer(), integer(), integer(), integer()},    % {q1, q2, q3, q4}
    parent      :: integer() | undefined,
    elements    :: []
}).

new(MinX, MinY, MaxX, MaxY, SplitTimes) when SplitTimes >= 0 ->
    Quad = create_quadrand(1, {MinX, MinY, MaxX, MaxY}, undefined),
    QuadMaps = maps:put(Quad#quad.id, Quad, maps:new()),
    do_create_quad(SplitTimes, Quad, QuadMaps).

do_create_quad(0, _, QuadMaps) ->
    QuadMaps;

do_create_quad(SplitTimes, #quad{id = Id, bounds = Bounds} = Quad, QuadMpas) ->
    QuadSize = maps:size(QuadMpas),
    {Bounds1, Bounds2, Bounds3, Bounds4} = split_bounds(Bounds),

    Quadrand1 = create_quadrand(QuadSize + 1, Bounds1, Id),
    Quadrand2 = create_quadrand(QuadSize + 2, Bounds2, Id),
    Quadrand3 = create_quadrand(QuadSize + 3, Bounds3, Id),
    Quadrand4 = create_quadrand(QuadSize + 4, Bounds4, Id),

    NewQuad = Quad#quad{
        quardrants = {
            Quadrand1#quad.id,
            Quadrand2#quad.id,
            Quadrand3#quad.id,
            Quadrand4#quad.id
        }
    },

    NewQuadMaps1 = maps:put(NewQuad#quad.id, NewQuad, QuadMpas),

    NewQuadMaps2 = lists:foldl(
        fun(Q, Acc) -> maps:put(Q#quad.id, Q, Acc) end,
        NewQuadMaps1,
        [Quadrand1, Quadrand2, Quadrand3, Quadrand4]
    ),



    NewQuadMaps3 = do_create_quad(SplitTimes-1, Quadrand1, NewQuadMaps2),
    NewQuadMaps4 = do_create_quad(SplitTimes-1, Quadrand2, NewQuadMaps3),
    NewQuadMaps5 = do_create_quad(SplitTimes-1, Quadrand3, NewQuadMaps4),
    NewQuadMaps6 = do_create_quad(SplitTimes-1, Quadrand4, NewQuadMaps5),

    NewQuadMaps6.


split_bounds({MinX, MinY, MaxX, MaxY}) ->
    CenterX = (MinX + MaxX) / 2,
    CenterY = (MinY + MaxY) / 2,

    {
        {CenterX, CenterY, MaxX, MaxY},
        {MinX, CenterY, CenterX, MaxY},
        {MinX, MinY, CenterX, CenterY},
        {CenterX, MinY, MaxX, CenterY}
    }.


create_quadrand(Id, Bounds, Parent) ->
    #quad{
        id = Id,
        bounds = Bounds,
        quardrants = {},
        parent = Parent,
        elements = []
    }.
