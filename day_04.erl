-module(day_04).
-import(binary, [split/2, split/3]).
-import(maps, [get/2]).
-export([solve/0, solve_timed/0]).

process_input(Path) ->
    {ok, File} = file:read_file(Path),
    [parse_line(Line) || Line <- split(File, <<"\n">>, [global])].

parse_contents(Contents) -> 
    [[Item || Item <- split(ContentPart, <<" ">>, [global]), Item =/= <<"">>] || ContentPart <- split(Contents, <<" | ">>)].

parse_line(Line) -> 
    [CardId, Contents] = split(Line, <<": ">>),
    #{id => CardId, contents => parse_contents(Contents)}.

check_my_numbers([], _Winning, Result) -> Result;
check_my_numbers([H|T], Winning, Result) when H == Winning -> 
    check_my_numbers(T, Winning, [H|Result]);
check_my_numbers([_H|T], Winning, Result) -> 
    check_my_numbers(T, Winning, Result).

check_winning_numbers([[], _MyNums], Result) -> lists:flatten(Result);
check_winning_numbers([[H|T], MyNums], Result) -> 
    check_winning_numbers([T, MyNums], [check_my_numbers(MyNums, H, []) | Result]).

winning_ns_to_points(Numbers) -> trunc(math:pow(2, length(Numbers)-1)).

duplicate_cards(CurrentCardWinningNs, Iteration, CountList) -> 
    CurrentCardScore = length(CurrentCardWinningNs),
    CurrentCardCount = lists:nth(Iteration, CountList),
    StartSection = lists:sublist(CountList, 1, Iteration),
    DuplicatedSection = [CardCount + CurrentCardCount || CardCount <- lists:sublist(CountList, Iteration+1, CurrentCardScore)],
    EndSection = lists:sublist(CountList, Iteration + 1 + CurrentCardScore, 99999),
    StartSection ++ DuplicatedSection ++ EndSection.

check_cards([], _Iteration, PointsCount, CardsCount) -> 
    {
        lists:sum(PointsCount),
        lists:sum(CardsCount)
    };
check_cards([H|T], Iteration, PointsCount, CardsCount) -> 
    Winning = check_winning_numbers(maps:get(contents, H), []),
    check_cards(
        T, Iteration + 1, 
        [winning_ns_to_points(Winning) | PointsCount], 
        duplicate_cards(Winning, Iteration, CardsCount)
    ).

solve() -> 
    Input = process_input("./data/day_04.aoc"),
    check_cards(Input, 1, [], lists:duplicate(length(Input), 1)).

solve_timed() -> util:timed(fun solve/0).