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

solve() -> 
    {Start, _Map} = process_import("./data/day_10.aoc"),
    {Start}.

solve_timed() -> util:timed(fun solve/0).