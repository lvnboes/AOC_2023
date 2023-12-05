-module(day_05).
-import(binary, [split/2, split/3]).
-export([solve/0, solve_timed/0]).

process_input(Path) ->
    {ok, File} = file:read_file(Path),
    [Seeds, ConversionMaps] = split(File, <<"\n\n">>),
    {
        [binary_to_integer(Seed) || Seed <- lists:droplast(lists:reverse(split(Seeds, <<" ">>, [global])))],
        [parse_conv_map(ConvMap) || ConvMap <- split(ConversionMaps, <<"\n\n">>, [global])]
    }.

parse_conv_map(ConvMap) -> 
    [Conv, Map] = split(ConvMap, <<":\n">>),
    {
        split(lists:nth(1, split(Conv, <<" ">>)), <<"-to-">>), 
        [[binary_to_integer(Val) || Val <- split(Line, <<" ">>, [global])] || Line <- split(Map, <<"\n">>, [global])]
    }.

find_destination(Seed, {_Header, []}) -> Seed;
find_destination(Seed, {_Header, [[Dest, Source, Range] | _Conversions]}) 
  when (Seed >= Source) andalso (Seed =< (Source + Range - 1)) ->
    Dest + Seed - Source;
find_destination(Seed, {Header, [_Conv | Conversions]}) -> 
    find_destination(Seed, {Header, Conversions}).

find_location(Seed, []) -> Seed;
find_location(Seed, [ConvTable | ConversionTables]) -> 
    find_location(find_destination(Seed, ConvTable), ConversionTables).

find_locations({[], _Tables}, Locations) -> Locations;
find_locations({[Seed | Seeds], Tables}, Locations) -> 
    find_locations({Seeds, Tables}, [find_location(Seed, Tables) | Locations]).

solve() -> 
    Input = process_input("./data/day_05.aoc"),
    #{
        part1 =>lists:min(find_locations(Input, [])),
        part2 => undefined
    }.

solve_timed() -> util:timed(fun solve/0).