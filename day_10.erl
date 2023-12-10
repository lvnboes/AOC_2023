-module(day_10).
-export([solve/0, solve_timed/0]).

process_import(Path) ->
    {ok, File} = file:read_file(Path),
    Map = [binary_to_list(Line) || Line <- binary:split(File, <<"\n">>, [global])],
    Start = find_start_location(Map),
    {Start, array:from_list([array:from_list(Line) || Line <- Map])}.

find_start_location(Map) -> find_start_location(Map, 1).
find_start_location([H|T], Line) ->
    case find_start_index(H) of
        not_found -> find_start_location(T, Line+1);
        Col -> {Line, Col}
    end.

find_start_index(Line) ->
    case lists:member($S, Line) of
        true -> {match, [{S,_L}]} = re:run(Line, "S"), S+1;
        false -> not_found
    end.

solve() -> 
    {Start, _Map} = process_import("./data/day_10.aoc"),
    {Start}.

solve_timed() -> util:timed(fun solve/0).