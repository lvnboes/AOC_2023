-module(day_10).
-export([solve/0, solve_timed/0]).

process_import(Path) ->
    {ok, File} = file:read_file(Path),
    Map = [binary_to_list(Line) || Line <- binary:split(File, <<"\n">>, [global])],
    Start = start_cooridnate(Map),
    {Start, array:from_list([array:from_list(Line) || Line <- Map])}.

start_cooridnate(Map) -> start_cooridnate(Map, 1).
start_cooridnate([H|T], Line) ->
    case start_line_index(H) of
        not_found -> start_cooridnate(T, Line+1);
        Col -> {Line, Col}
    end.

start_line_index(Line) ->
    case lists:member($S, Line) of
        true -> {match, [{S,_L}]} = re:run(Line, "S"), S+1;
        false -> not_found
    end.

find_next(LastLoc, CurrentLoc, CountLocs, PastLocs, Map) ->
    {LastX, LastY, LastSign} = LastLoc,
    {CurrentX, CurrentY, CurrentSign} = CurrentLoc,
    if
        LastSign == $- andalso CurrentSign == $7 andalso CurrentX > LastX ->
            NextSign = array:get(CurrentX, array:get(CurrentY-1, Map)),
            find_next(CurrentLoc, {CurrentX, CurrentY-1, NextSign}, CountLocs+1, [CurrentLoc|PastLocs], Map);
        LastSign == $- andalso CurrentSign == $F andalso CurrentX < LastX -> undefined;
        LastSign == $- andalso CurrentSign == $L andalso CurrentX < LastX -> undefined;
        LastSign == $- andalso CurrentSign == $J andalso CurrentX > LastX -> undefined;
        LastSign == $- andalso CurrentSign == $- -> undefined;

        LastSign == $| andalso CurrentSign == $7 andalso CurrentY < LastY -> undefined;
        LastSign == $| andalso CurrentSign == $F andalso CurrentY < LastY -> undefined;
        LastSign == $| andalso CurrentSign == $L andalso CurrentY > LastY -> undefined;
        LastSign == $| andalso CurrentSign == $J andalso CurrentY > LastY -> undefined;
        LastSign == $| andalso CurrentSign == $| -> undefined;

        LastSign == $7 andalso CurrentSign == $F andalso CurrentX < LastX -> undefined;
        LastSign == $7 andalso CurrentSign == $L -> undefined;
        LastSign == $7 andalso CurrentSign == $J andalso CurrentY > LastY -> undefined;
        LastSign == $7 andalso CurrentSign == $- andalso CurrentX < LastX -> undefined;
        LastSign == $7 andalso CurrentSign == $| andalso CurrentY > LastY -> undefined;

        LastSign == $F andalso CurrentSign == $7 andalso CurrentX > LastX -> undefined;
        LastSign == $F andalso CurrentSign == $L andalso CurrentY > LastY -> undefined;
        LastSign == $F andalso CurrentSign == $J -> undefined;
        LastSign == $F andalso CurrentSign == $- andalso CurrentX > LastX -> undefined;
        LastSign == $F andalso CurrentSign == $| andalso CurrentY > LastY -> undefined;

        LastSign == $L andalso CurrentSign == $7 -> undefined;
        LastSign == $L andalso CurrentSign == $F andalso CurrentY < LastY -> undefined;
        LastSign == $L andalso CurrentSign == $J andalso CurrentX > LastX -> undefined;
        LastSign == $L andalso CurrentSign == $- andalso CurrentX > LastX -> undefined;
        LastSign == $L andalso CurrentSign == $| andalso CurrentY < LastY -> undefined;

        LastSign == $J andalso CurrentSign == $7 andalso CurrentY < LastY -> undefined;
        LastSign == $J andalso CurrentSign == $F -> undefined;
        LastSign == $J andalso CurrentSign == $L andalso CurrentX < LastX -> undefined;
        LastSign == $J andalso CurrentSign == $- andalso CurrentX < LastX -> undefined;
        LastSign == $J andalso CurrentSign == $| andalso CurrentY < LastY -> undefined;

        LastSign == $S andalso CurrentSign == $7 andalso (CurrentX > LastX orelse CurrentY < LastY) -> undefined;
        LastSign == $S andalso CurrentSign == $F andalso (CurrentX < LastX orelse CurrentY > LastY)  -> undefined;
        LastSign == $S andalso CurrentSign == $L andalso (CurrentX < LastX orelse CurrentY < LastY)  -> undefined;
        LastSign == $S andalso CurrentSign == $J andalso (CurrentX > LastX orelse CurrentY > LastY)  -> undefined;
        LastSign == $S andalso CurrentSign == $- andalso CurrentY == LastY -> undefined;
        LastSign == $S andalso CurrentSign == $| andalso CurrentX == LastX -> undefined;

        true -> no_path
    end.



solve() -> 
    {Start, _Map} = process_import("./data/day_10.aoc"),
    {Start}.

solve_timed() -> util:timed(fun solve/0).