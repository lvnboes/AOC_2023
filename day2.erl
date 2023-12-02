-module(day2).
-export([solve/0, solve_timed/0]).

%Reading and parsing input

process_input(Path) ->
    {ok, File} = file:read_file(Path),
    Lines = [[Test || Test <- binary:split(Line, <<":">>)] || Line <- binary:split(File, <<"\n">>, [global])],
    Games = [
        {binary_to_integer(lists:nth(2, binary:split(Id, <<" ">>))), parse_game(binary:split(Game, <<";">>, [global]), [])}
        || [Id, Game] <- Lines
    ],
    Games.

parse_cube([_, Amount, Colour]) -> {Colour, binary_to_integer(Amount)}.

parse_set([], Parsed) -> maps:from_list(Parsed);
parse_set([Cube|Cubes], Parsed) -> parse_set(Cubes, [parse_cube(binary:split(Cube, <<" ">>, [global])) | Parsed]).

parse_game([], Parsed) -> Parsed;
parse_game([Set|Sets], Parsed) -> parse_game(Sets, [parse_set(binary:split(Set, <<",">>, [global]), []) | Parsed]).

%Part 1

check_set(_Set, _Allowed, []) -> true;
check_set(Set, Allowed, [Colour|Colours]) -> 
    ValidColour = (not maps:is_key(Colour, Set)) orelse (maps:get(Colour, Set) =< maps:get(Colour, Allowed)),
    if
        ValidColour -> check_set(Set, Allowed, Colours);
        true -> false
    end.

check_game([], _Allowed, _Colours) -> true;
check_game([Set|Sets], Allowed, Colours) ->
    ValidSet = check_set(Set, Allowed, Colours),
    if 
        ValidSet -> check_game(Sets, Allowed, Colours);
        true -> false
    end.

find_allowed_games([], _Allowed, _Colours, Result) -> Result;
find_allowed_games([{Id, Game}|Games], Allowed, Colours, Result) ->
    Possible = check_game(Game, Allowed, Colours),
    if 
        Possible -> find_allowed_games(Games, Allowed, Colours, [Id|Result]);
        true -> find_allowed_games(Games, Allowed, Colours, Result)
    end.

%Part 2

get_colour_val(Colour, Set) ->
    IsKey = maps:is_key(Colour, Set),
    if
        IsKey -> maps:get(Colour, Set);
        true -> 0
    end.

max_val_in_game(_Colour, [], Results) -> lists:max(Results);
max_val_in_game(Colour, [Set|Sets], Results) -> max_val_in_game(Colour, Sets, [get_colour_val(Colour, Set) | Results]).

min_set(_Game, [], Result) -> Result;
min_set(Game, [Colour|Colours], Result) -> min_set(Game, Colours, [max_val_in_game(Colour, Game, []) | Result]).

set_power([R, G, B]) -> R*G*B.

get_powers(Games, _Allowed, Colours, Result) -> get_powers(Games, Colours, Result).
get_powers([], _Colours, Result) -> Result;
get_powers([{_Id, Game}|Games], Colours, Result) -> get_powers(Games, Colours, [set_power(min_set(Game, Colours, [])) | Result]).

%Common

sum_results(Games, Allowed, Colours, F) -> lists:sum(F(Games, Allowed, Colours, [])).

solve() ->
    Input = process_input("./data/d2.aoc"),
    Allowed = #{<<"red">> => 12, <<"green">> => 13, <<"blue">> => 14},
    Colours = maps:keys(Allowed),
    {
        {part1,sum_results(Input, Allowed, Colours, fun find_allowed_games/4)},
        {part2,sum_results(Input, Allowed, Colours, fun get_powers/4)}
    }.

solve_timed() -> util:timed(fun() -> solve() end).