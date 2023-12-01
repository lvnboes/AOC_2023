-module(day1).
-import(binary, [split/3]).
-import(maps, [get/2, is_key/2]).
-import(lists, [last/1, sublist/3, nth/2]).
-import(util, [timed/1]).
-export([run/0]).

process_input(Path) ->
    {ok, File} = file:read_file(Path),
    [binary_to_list(Line) || Line <- split(File, <<"\n">>, [global])].

find_ints([], _Map, AllNums) -> AllNums;
find_ints([H|T], Map, AllNums) -> 
    PossibleNums = [[H], sublist([H|T],1,3), sublist([H|T],1,4), sublist([H|T],1,5)],
    Nums = [Num || Num <- PossibleNums , is_key(Num, Map)],
    if
        length(Nums) > 0 -> find_ints(T, Map, [get(nth(1,Nums), Map)|AllNums]);
        true -> find_ints(T, Map, AllNums)
    end.

get_calibration(Line, Map) -> 
    [H|T] = find_ints(Line, Map, []),
    list_to_integer([last([H|T]),H]).

sum_calibration([], _PatternMap) -> 0;
sum_calibration([H|T], PatternMap) -> get_calibration(H, PatternMap) + sum_calibration(T, PatternMap).

run() -> 
    Input = process_input("./data/d1.aoc"),
    Map1 = #{
        "0" => $0, "1" => $1, "2" => $2, "3" => $3, "4" => $4 , "5" => $5 , 
        "6" => $6 , "7" => $7, "8" => $8, "9" => $9
    },
    Map2 = Map1#{
        "one" => $1, "two" => $2, "three" => $3, "four" => $4, "five" => $5, 
        "six" => $6, "seven" => $7, "eight" => $8, "nine" => $9
    },
    timed(fun() -> {{part1, sum_calibration(Input, Map1)}, {part2, sum_calibration(Input, Map2)}} end).