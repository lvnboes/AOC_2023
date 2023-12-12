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
        CurrentSign == $7 andalso (LastSign == $- orelse LastSign == $L orelse LastSign == $F orelse LastSign == $S) -> undefined;
        CurrentSign == $7 andalso (LastSign == $| orelse LastSign == $J orelse LastSign == $L orelse LastSign == $S) -> undefined;
        CurrentSign == $J andalso (LastSign == $- orelse LastSign == $L orelse LastSign == $F orelse LastSign == $S) -> undefined;
        CurrentSign == $J andalso (LastSign == $| orelse LastSign == $J orelse LastSign == $L orelse LastSign == $S) -> undefined;
        CurrentSign == $L andalso (LastSign == $- orelse LastSign == $7 orelse LastSign == $J orelse LastSign == $S) -> undefined;
        CurrentSign == $L andalso (LastSign == $| orelse LastSign == $7 orelse LastSign == $F orelse LastSign == $S) -> undefined;
        CurrentSign == $F andalso (LastSign == $- orelse LastSign == $7 orelse LastSign == $J orelse LastSign == $S) -> undefined;
        CurrentSign == $F andalso (LastSign == $| orelse LastSign == $J orelse LastSign == $L orelse LastSign == $S) -> undefined;
        CurrentSign == $- andalso (LastSign == $- orelse ((LastSign == $F orelse LastSign == $L) andalso LastX < CurrentX) orelse ((LastSign == $J orelse LastSign == $7) andalso LastX > CurrentX) orelse LastSign == $S) -> undefined;
        CurrentSign == $| andalso (LastSign == $| orelse ((LastSign == $F orelse LastSign == $7) andalso LastY < CurrentY) orelse ((LastSign == $L orelse LastSign == $J) andalso LastY > CurrentY) orelse LastSign == $S) -> undefined;
        LastSign == $S -> undefined;

        true -> no_path
    end.



solve() -> 
    {Start, _Map} = process_import("./data/day_10.aoc"),
    {Start}.

solve_timed() -> util:timed(fun solve/0).