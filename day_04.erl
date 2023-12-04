-module(day_04).
-import(lists, [nth/2]).
-import(binary, [split/2, split/3]).
-import(maps, [get/2]).
-export([solve/0, solve_timed/0]).

process_input(Path) ->
    {ok, File} = file:read_file(Path),
    [parse_line(Line) || Line <- split(File, <<"\n">>, [global])].

parse_contents(Contents) -> [[Item || Item <- split(ContentPart, <<" ">>, [global]), Item =/= <<"">>] || ContentPart <- split(Contents, <<" | ">>)].
%    [Winning, Mine] = split(Contents, <<" | ">>),
%    #{winning => split(Winning, <<" ">>, [global]), mine => split(Mine, <<" ">>, [global])}.

parse_line(Line) -> 
    [CardId, Contents] = split(Line, <<": ">>),
    #{id => CardId, contents => parse_contents(Contents)}.

check_my_numbers([], _Winning, Result) -> Result;
check_my_numbers([H|T], Winning, Result) when H == Winning -> 
    check_my_numbers(T, Winning, [H|Result]);
check_my_numbers([_H|T], Winning, Result) -> 
    check_my_numbers(T, Winning, Result).

check_winning_numbers([[], _MyNums], Result) -> Result;
check_winning_numbers([[H|T], MyNums], Result) -> 
    check_winning_numbers([T, MyNums], [check_my_numbers(MyNums, H, []) | Result]).

numbers_to_points(Numbers) -> trunc(math:pow(2, length(Numbers)-1)).

check_cards([], Result) -> Result;
check_cards([H|T], Result) -> 
    Points = numbers_to_points(
        lists:flatten(check_winning_numbers(maps:get(contents, H), []))
    ),
    check_cards(T, [Points | Result]).

solve() -> 
    Input = process_input("./data/day_04.aoc"),
    util:print(lists:sum(check_cards(Input, []))).

solve_timed() -> util:timed(fun solve/0).