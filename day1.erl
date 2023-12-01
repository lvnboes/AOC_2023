-module(day1).
-import(binary, [split/3]).
-import(maps, [get/2, is_key/2]).
-import(lists, [last/1, sublist/3, nth/2]).
-export([solve/0]).

process_input(Path) ->
    {ok, File} = file:read_file(Path),
    [binary_to_list(Line) || Line <- split(File, <<"\n">>, [global])].

find_ints([], _Map, Ints) -> Ints;
find_ints([H|T], Map, Ints) -> 
    Window = [
        {[H], is_key([H], Map)}, 
        {sublist([H|T],1,3), is_key(sublist([H|T],1,3), Map)}, 
        {sublist([H|T],1,4), is_key(sublist([H|T],1,4), Map)}, 
        {sublist([H|T],1,5), is_key(sublist([H|T],1,5), Map)}
    ],
    Matches = [Key || {Key, IsKey} <- Window , IsKey],
    if
        length(Matches) > 0 -> find_ints(T, Map, [get(nth(1,Matches), Map)|Ints]);
        true -> find_ints(T, Map, Ints)
    end.

get_calibration(Line, Map) -> 
    [H|T] = find_ints(Line, Map, []),
    list_to_integer([last([H|T]),H]).

sum_calibration([], _PatternMap) -> 0;
sum_calibration([H|T], PatternMap) -> 
    Cal = get_calibration(H, PatternMap),
    Cal + sum_calibration(T, PatternMap).

solve() -> 
    Input = process_input("./input/d1.aoc"),
    Map1 = #{"0" => $0, "1" => $1, "2" => $2, "3" => $3, "4" => $4 , "5" => $5 , "6" => $6 , "7" => $7, "8" => $8, "9" => $9},
    Map2 = Map1#{"one" => $1, "two" => $2, "three" => $3, "four" => $4, "five" => $5, "six" => $6, "seven" => $7, "eight" => $8, "nine" => $9},
    {
        {part1, sum_calibration(Input, Map1)}, 
        {part2, sum_calibration(Input, Map2)}
    }.