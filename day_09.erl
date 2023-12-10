-module(day_09).
-export([solve/0, solve_timed/0]).

%Reading and parsing input

process_input(Path) ->
    {ok, File} = file:read_file(Path),
    [[binary_to_integer(Value) || Value <- binary:split(Line, <<" ">>, [global])] || Line <- binary:split(File, <<"\n">>, [global])].

%Part 2

extrapolate_past(FirstValues) -> extrapolate_past(FirstValues, 0).
extrapolate_past([], Ex) -> Ex;
extrapolate_past([H | T], Ex) -> extrapolate_past(T, H-Ex).

%Common

differences(Values) -> differences(Values, []).
differences([_A], Diffs) -> lists:reverse(Diffs);
differences([A, B | T], Diffs) -> differences([B | T], [B-A | Diffs]).

extrapolate(Values) -> extrapolate(Values, [] ,lists:last(Values)).
extrapolate(Values, FirstVals, SumLast) ->
    case lists:sum(Values) of
        0 -> {extrapolate_past(FirstVals) , SumLast};
        _ ->
            [First | _Rest] = Values,
            Diffs = differences(Values),
            extrapolate(Diffs, [First | FirstVals], SumLast + lists:last(Diffs))
    end.

summed_extrapolations(Data) -> summed_extrapolations(Data, 0, 0).
summed_extrapolations([], Past, Future) -> #{part1 => Future, part2 => Past};
summed_extrapolations([H | T], Past, Future) -> 
    {P, F} = extrapolate(H),
    summed_extrapolations(T, Past+P, Future+F).

solve() -> 
    Input = process_input("./data/day_09.aoc"),
    summed_extrapolations(Input).

solve_timed() -> util:timed(fun solve/0).