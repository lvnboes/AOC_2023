-module(day01).
-export([solve/0, solve_timed/0]).

%Reading and parsing input

process_input(Path) ->
    {ok, File} = file:read_file(Path),
    [binary_to_list(Line) || Line <- binary:split(File, <<"\n">>, [global])].

%Common

find_nums([], _Map, AllNums) -> AllNums;
find_nums([H|T], Map, AllNums) -> 
    PossibleNums = [[H], lists:sublist([H|T],1,3), lists:sublist([H|T],1,4), lists:sublist([H|T],1,5)],
    Nums = [Num || Num <- PossibleNums , maps:is_key(Num, Map)],
    if
        length(Nums) > 0 -> find_nums(T, Map, [maps:get(lists:nth(1,Nums), Map)|AllNums]);
        true -> find_nums(T, Map, AllNums)
    end.

get_calibration(Line, Map) -> 
    [H|T] = find_nums(Line, Map, []),
    lists:last([H|T])*10+H.

sum_calibration([], _PatternMap) -> 0;
sum_calibration([H|T], PatternMap) -> get_calibration(H, PatternMap) + sum_calibration(T, PatternMap).

solve() -> 
    Input = process_input("./data/day01.aoc"),
    Map1 = #{
        "0" => 0, "1" => 1, "2" => 2, "3" => 3, "4" => 4 , "5" => 5 , "6" => 6 , "7" => 7, "8" => 8, "9" => 9
    },
    Map2 = Map1#{
        "one" => 1, "two" => 2, "three" => 3, "four" => 4, "five" => 5, "six" => 6, "seven" => 7, "eight" => 8, "nine" => 9
    },
    #{part1 => sum_calibration(Input, Map1), part2 => sum_calibration(Input, Map2)}.

solve_timed() -> util:timed(fun solve/0).