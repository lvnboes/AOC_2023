-module(day_05).
-import(binary, [split/2, split/3]).
-export([solve/0, solve_timed/0]).

process_input(Path) ->
    {ok, File} = file:read_file(Path),
    [Seeds, ConversionMaps] = split(File, <<"\n\n">>),
    {
        lists:droplast(lists:reverse(split(Seeds, <<" ">>, [global]))),
        [parse_conv_map(ConvMap) || ConvMap <- split(ConversionMaps, <<"\n\n">>, [global])]
    }.

parse_conv_map(ConvMap) -> 
    [Conv, Map] = split(ConvMap, <<":\n">>),
    {
        split(lists:nth(1, split(Conv, <<" ">>)), <<"-to-">>), 
        [[binary_to_integer(Val) || Val <- split(Line, <<" ">>, [global])] || Line <- split(Map, <<"\n">>, [global])]
    }.

solve() -> process_input("./data/test.aoc").
solve_timed() -> util:timed(fun solve/0).