-module(day_06).
-export([solve/0, solve_timed/0]).

%Reading and basic parsing of input

process_input(Path) ->
    {ok, File} = file:read_file(Path),
    binary:split(File, <<"\n">>).

%Part 1

parse_part_1(Input) -> 
    [Times, Dists] = [
        [
            binary_to_integer(N) 
            || N <- lists:sublist(
                [Seg || Seg <- binary:split(Line, <<" ">>, [global]), Seg =/= <<"">>], 
                2, 9999
            )
        ] || Line <- Input
    ],
    lists:zip(Times, Dists).

ways_to_win_multiplied(TimesAndDists) -> 
    multiply_all([ways_to_win(TimeAndDist) || TimeAndDist <- TimesAndDists]).

multiply_all([]) -> 1;
multiply_all([H|T]) -> H * multiply_all(T).

%Part 2

parse_part_2(Input) ->
    list_to_tuple([
        binary_to_integer(
            binary:replace(
                lists:nth(
                    2, 
                    binary:split(Line, <<" ">>)
                ), 
                <<" ">>, <<"">>, [global]
            )
        ) || Line <- Input
    ]).

%Common

ways_to_win({Time, Dist}) -> 
    SqrtDisc = math:sqrt(math:pow(Time, 2) - 4*Dist),
    trunc(((-Time - SqrtDisc) / -2) - 0.0001) - round(((-Time + SqrtDisc) / -2) + 0.5) + 1.

solve() ->
    Input = process_input("./data/day_06.aoc"),
    #{
        part1 => ways_to_win_multiplied(parse_part_1(Input)),
        part2 => ways_to_win(parse_part_2(Input))
    }.

solve_timed() -> util:timed(fun solve/0).