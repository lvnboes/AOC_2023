-module(day_07).
-export([solve/0, solve_timed/0]).

%Reading and parsing input

process_input(Path) ->
    {ok, File} = file:read_file(Path),
    Lines = binary:split(
        binary:replace(
            binary:replace(
                binary:replace(
                    binary:replace(
                        binary:replace(
                            File, <<"A">>, <<"e">>, [global]
                        ), <<"K">>, <<"d">>, [global]
                    ), <<"Q">>, <<"c">>, [global]
                ), <<"J">>, <<"b">>, [global]
            ), <<"T">>, <<"a">>, [global]
        ), <<"\n">>, [global]
    ),
    [parse_line(binary:split(Line, <<" ">>)) || Line <- Lines].

parse_line([Hand, Stake]) -> {binary_to_list(Hand), binary_to_integer(Stake)}.

%Common

count_winnings(Hands) -> count_winnings(Hands, 0, 1).
count_winnings([], Winnings, _Rank) -> Winnings;
count_winnings([{_Hand, Stake, _Score, _Rule}|Hands], Winnings, Rank) -> 
    count_winnings(Hands, Winnings + Rank * Stake, Rank + 1).

hand_sorter({Hand1, _Stake1, Score1, Rule}, {Hand2, _Stake2, Score2, Rule}) ->
    if 
        Score1 == Score2 -> 
            if 
                Rule == joker ->
                    H1J = string:replace(Hand1, "b", "0", all),
                    H2J = string:replace(Hand2, "b", "0", all),
                    H1J < H2J;
                true -> Hand1 < Hand2
            end;
        true -> Score1 < Score2
    end.

score_by_rule(Hands, Rule) -> 
    [{Hand, Stake, score(Hand, Rule), Rule} || {Hand, Stake} <- Hands].

score(Hand, Rule) ->
    case card_counts(Hand, Rule) of
        [5 | _T] -> 7;
        [4 | _T] -> 6;
        [3, 2 | _T] -> 5;
        [3 | _T] -> 4;
        [2, 2 | _T] -> 3;
        [2 | _T] -> 2;
        [1 | _T] -> 1
    end.

card_counts(Hand, Rule) -> card_counts(Hand, Rule, #{}).
card_counts([], Rule, CountMap) -> 
    case (Rule == joker andalso maps:is_key($b, CountMap)) of
        true -> 
            Jokers = maps:get($b, CountMap),
            CountMapByRule = CountMap#{$b => 0};
        false -> 
            Jokers = 0,
            CountMapByRule = CountMap
    end,
    [H | T] = lists:sort(
        fun(A, B) -> A > B end,
        [Count || {_Card, Count} <- maps:to_list(CountMapByRule)]
    ),
    [H+Jokers | T];
card_counts([H | T], Rule, CountMap) -> 
    case maps:is_key(H, CountMap) of
        true -> card_counts(T, Rule, CountMap#{H => maps:get(H, CountMap)+1});
        false -> card_counts(T, Rule, CountMap#{H => 1})
    end.

solve() -> 
    Input = process_input("./data/day_07.aoc"),
    Part1 = count_winnings(
        lists:sort(
            fun hand_sorter/2,
            score_by_rule(Input, no_joker)
        )
    ),
    Part2 = count_winnings(
        lists:sort(
            fun hand_sorter/2,
            score_by_rule(Input, joker)
        )
    ),
    #{part1 => Part1, part2 => Part2}.

solve_timed() -> util:timed(fun solve/0).