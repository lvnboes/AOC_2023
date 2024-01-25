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
        CurrentSign == $- andalso (
            LastSign == $- 
            orelse ((LastSign == $F orelse lastSign == $L) andalso LastX < CurrentX)
            orelse ((LastSign == $J orelse LastSign == $7) andalso LastX > CurrentX)
        ) -> 
            NextX = CurrentX + (CurrentX - LastX),
            NextLoc = {NextX, CurrentY, array:get(NextX, array:get(CurrentY, Map))},
            find_next(CurrentLoc, NextLoc, CountLocs+1, [CurrentLoc | PastLocs], Map);
        CurrentSign == $| andalso (
            LastSign == $| 
            orelse ((LastSign == $F orelse lastSign == $7) andalso LastY < CurrentY)
            orelse ((LastSign == $J orelse LastSign == $L) andalso LastY > CurrentY)
        ) -> 
            NextY = CurrentY + (CurrentY - LastY),
            NextLoc = {CurrentX, NextY, array:get(CurrentX, array:get(NextY, Map))},
            find_next(CurrentLoc, NextLoc, CountLocs+1, [CurrentLoc | PastLocs], Map);
        CurrentSign == $7 andalso (
            (
                LastSign == $- orelse LastSign == $L orelse LastSign == $F
                andalso LastX < CurrentX
            )
            orelse(
                (LastSign == $| orelse LastSign == $L orelse LastSign == $J) 
                andalso LastY > CurrentY
            )
        ) -> 
            NextX = CurrentX + (CurrentY - LastY),
            NextY = CurrentY + (CurrentX - LastX),
            NextLoc = {NextX, NextY, array:get(NextX, array:get(NextY, Map))},
            find_next(CurrentLoc, NextLoc, CountLocs+1, [CurrentLoc | PastLocs], Map);
        CurrentSign == $J andalso (
            (
                LastSign == $- orelse LastSign == $L orelse LastSign == $F
                andalso LastX < CurrentX
            )
            orelse(
                (LastSign == $| orelse LastSign == $7 orelse LastSign == $F) 
                andalso LastY < CurrentY
            )
        ) -> undefined;
        CurrentSign == $L andalso (
            (
                LastSign == $- orelse LastSign == $J orelse LastSign == $7
                andalso LastX > CurrentX
            )
            orelse(
                (LastSign == $| orelse LastSign == $F orelse LastSign == $7) 
                andalso LastY < CurrentY
            )
        ) -> undefined;
        CurrentSign == $F andalso (
            (
                LastSign == $- orelse LastSign == $J orelse LastSign == $7
                andalso LastX > CurrentX
            )
            orelse(
                (LastSign == $| orelse LastSign == $J orelse LastSign == $L) 
                andalso LastY > CurrentY
            )
        ) -> undefined;
        LastSign == $S -> undefined;
        CurrentSign == $S -> undefined;
        true -> no_path
    end.


solve() -> 
    {Start, _Map} = process_import("./data/day_10.aoc"),
    {Start}.

solve_timed() -> util:timed(fun solve/0).