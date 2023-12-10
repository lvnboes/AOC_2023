-module(day_10).
-export([solve/0, solve_timed/0]).

process_import(Path) ->
    {ok, File} = file:read_file(Path),
    [binary_to_list(Line) || Line <- binary:split(File, <<"\n">>, [global])].

solve() -> 
    process_import("./data/day_10.aoc").

solve_timed() -> util:timed(fun solve/0).