-module(day_05).
-export([solve/0, solve_timed/0]).

%Reading and parsing input

process_input(Path) ->
    {ok, File} = file:read_file(Path),
    [Seeds, ConversionMaps] = binary:split(File, <<"\n\n">>),
    {
        [binary_to_integer(Seed) || Seed <- lists:droplast(lists:reverse(binary:split(Seeds, <<" ">>, [global])))],
        [parse_conv_map(ConvMap) || ConvMap <- binary:split(ConversionMaps, <<"\n\n">>, [global])]
    }.

parse_conv_map(ConvMap) -> 
    [_Conv, Map] = binary:split(ConvMap, <<":\n">>),
    lists:sort(
        fun conv_map_sorter/2, 
        [[binary_to_integer(Val) || Val <- binary:split(Line, <<" ">>, [global])] || Line <- binary:split(Map, <<"\n">>, [global])]
    ).

conv_map_sorter([_Dest1, Source1, _Range1], [_Dest2, Source2, _Range2]) -> Source1 =< Source2.

%Part 1

find_destination(Seed, []) -> Seed;
find_destination(Seed, [[Dest, Source, Range] | _Conversions])
  when (Seed >= Source) andalso (Seed < (Source + Range)) ->
    Dest + Seed - Source;
find_destination(Seed, [_Conv | Conversions]) -> 
    find_destination(Seed, Conversions).

find_location(Seed, []) -> Seed;
find_location(Seed, [ConvTable | ConversionTables]) -> 
    find_location(find_destination(Seed, ConvTable), ConversionTables).

find_locations([], _Tables, Locations) -> Locations;
find_locations([Seed | Seeds], Tables, Locations) -> 
    find_locations(Seeds, Tables, [find_location(Seed, Tables) | Locations]).

%Part 2

find_dest_ranges({Start, End}, [], DestRanges) -> [{Start, End} | DestRanges];
find_dest_ranges({Start, End}, [[Dest, Source, Range] | Conversions], DestRanges)
  when (Start >= Source) andalso (Start < (Source + Range)) ->
    DestStart = Dest + Start - Source,
    if 
        (Source + Range - 1) >= End -> [{DestStart, Dest + End - Source}];
        true -> find_dest_ranges({Source + Range, End}, Conversions, [{DestStart, Source + Range -1} | DestRanges])
    end;
find_dest_ranges(Range, [_Conversion | Conversions], DestRanges) -> 
    find_dest_ranges(Range, Conversions, DestRanges).

find_loc_range(Range, []) -> Range;
find_loc_range({Start, End}, [ConvTable | ConversionTables]) ->
    DestRanges = find_dest_ranges({Start, End}, ConvTable, []),
    find_loc_ranges(DestRanges, ConversionTables, []).

find_loc_ranges([], _Tables, LocRanges) -> LocRanges;
find_loc_ranges([Range | Ranges], Tables, LocRanges) -> 
    find_loc_ranges(Ranges, Tables, lists:flatten([find_loc_range(Range, Tables) | LocRanges])).

to_seed_ranges(Seeds) -> to_seed_ranges(Seeds, []).
to_seed_ranges([], Ranges) -> Ranges;
to_seed_ranges([Length, Start | SeedRanges], Ranges) -> 
    to_seed_ranges(SeedRanges, [{Start, Start + Length -1} | Ranges]).

%Common

solve() -> 
    {Seeds, Tables} = process_input("./data/day_05.aoc"),
    #{
        part1 => lists:min(find_locations(Seeds, Tables, [])),
        part2 => lists:min([Start || {Start, _End} <- find_loc_ranges(to_seed_ranges(Seeds), Tables, [])])
    }.

solve_timed() -> util:timed(fun solve/0).