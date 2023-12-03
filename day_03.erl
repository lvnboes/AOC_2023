-module(day_03).
-export([solve/0, solve_timed/0]).

%Reading and parsing input

process_input(Path) ->
    {ok, File} = file:read_file(Path),
    [binary_to_list(Line) || Line <- binary:split(File, <<"\n">>, [global])].

numchar_to_int(Char) -> Char - $0.

is_numeric(X) -> (X >= $0) andalso (X =< $9).

is_marker(X) -> X =/= $. andalso X =/= $* andalso (X < $0 orelse X > $9).

is_gear(X) -> X == $*.

parse_char(_Char, []) -> no_match;
parse_char(Char, [{Type, Parser}|T]) ->
    case Parser(Char) of
        true -> Type;
        false -> parse_char(Char, T)
    end.

consolidate_number(Buffer) -> 
    Index = [I || {I, _Value} <- Buffer],
    consolidate_number(Buffer, lists:min(Index), lists:max(Index), 0, 0).
consolidate_number([], MinI, MaxI, _InternalI, Acc) -> {numeric, {MinI, MaxI}, Acc};
consolidate_number([{_I, Value}|Buffer], MinI, MaxI, InternalI, Acc) -> 
    consolidate_number(
        Buffer, MinI, MaxI, InternalI + 1, 
        trunc(Acc + math:pow(10, InternalI) * Value)
    ).

parse_line(String, Parsers) -> parse_line(String, Parsers, 0, [], [], [], []).
parse_line([], _Parsers, _I, NumericBuffer, Nums, Marks, Gears) 
  when length(NumericBuffer) == 0 -> 
    #{numeric => Nums, marker => Marks, gear => Gears};
parse_line([], _Parsers, _I, NumericBuffer, Nums, Marks, Gears) -> 
    #{numeric => [consolidate_number(NumericBuffer) | Nums], marker => Marks, gear => Gears};
parse_line([Char|T], Parsers, I, NumericBuffer, Nums, Marks, Gears) -> 
    BufferIsEmpty = length(NumericBuffer) == 0,
    Match = parse_char(Char, Parsers),
    if
        Match == numeric -> 
            parse_line(
                T, Parsers, I+1,
                [{I, numchar_to_int(Char)} | NumericBuffer], 
                Nums, Marks, Gears
            );
        Match == no_match andalso BufferIsEmpty -> 
            parse_line(T, Parsers, I+1, [], Nums, Marks, Gears);
        Match == no_match -> 
            parse_line(
                T, Parsers, I+1, [], 
                [consolidate_number(NumericBuffer) | Nums], 
                Marks, Gears
            );
        Match == marker andalso BufferIsEmpty -> 
            parse_line(
                T, Parsers, I+1, [], Nums, 
                [{marker, I} | Marks], 
                Gears
            );
        Match == marker ->
            parse_line(
                T, Parsers, I+1, [], 
                [consolidate_number(NumericBuffer) | Nums], 
                [{marker, I} | Marks], Gears
            );
        Match == gear andalso BufferIsEmpty -> 
            parse_line(
                T, Parsers, I+1, [], Nums, 
                [{marker, I} | Marks], 
                [{gear, I} | Gears]
            );
        Match == gear ->
            parse_line(
                T, Parsers, I+1, [], 
                [consolidate_number(NumericBuffer) | Nums], 
                [{marker, I} | Marks], 
                [{gear, I} | Gears]
            )
    end.

parse_lines(Lines) -> parse_lines(Lines, []).
parse_lines([], Result) -> Result;
parse_lines([Line|T], Result) -> 
    parse_lines(
        T, 
        [
            parse_line(
                Line,
                [
                    {numeric, fun is_numeric/1}, 
                    {marker, fun is_marker/1}, 
                    {gear, fun is_gear/1}
                ]
            ) 
            | Result
        ]
    ).

%Common

check_numbers(_Marker, [], Result) -> lists:flatten(Result);
check_numbers(Marker, [{numeric, {I1, I2}, Value}|T], Result) 
  when (Marker >= I1-1) andalso (Marker =< I2+1) -> 
    check_numbers(Marker, T, [Value | Result]);
check_numbers(Marker, [_H|T], Result) ->     
    check_numbers(Marker, T, Result).

find_marker_adjacent([], _Numbers, Result) -> lists:flatten(Result);
find_marker_adjacent([H|T], Numbers, Result) -> 
    find_marker_adjacent(T, Numbers, [check_numbers(H, Numbers, []) | Result]).

find_gear_adjecent_numbers([], _Numbers, Result) -> 
    [lists:nth(1, GAN) * lists:nth(2, GAN)|| GAN <- Result, length(GAN) == 2];
find_gear_adjecent_numbers([H|T], Numbers, Result) -> 
    find_gear_adjecent_numbers(T, Numbers, [check_numbers(H, Numbers, []) | Result]).

both_parts(Input, I, Result1, Result2) when length(Input) < I ->
    #{
        part1 => lists:sum(lists:flatten(Result1)), 
        part2 => lists:sum(lists:flatten(Result2))
    };
both_parts(Input, I, Result1, Result2) -> 
    From = max(1, (I -1)),
    To = min(length(Input), (I+1)),
    Window = lists:sublist(Input, From, (To-From+1)),
    Markers3L = [Ind || {_Marker, Ind} <- lists:flatten([maps:get(marker, Line) || Line <- Window])],
    Numbers1L = maps:get(numeric, lists:nth(I, Input)),
    Gears1L = [Ind || {_Gear, Ind} <- maps:get(gear, lists:nth(I, Input))],
    Numbers3L = lists:flatten([maps:get(numeric, Line) || Line <- Window]),
    both_parts(
        Input, I + 1, 
        [find_marker_adjacent(Markers3L, Numbers1L, []) | Result1], 
        [find_gear_adjecent_numbers(Gears1L, Numbers3L, []) | Result2]
    ).

solve() -> 
    Input = process_input("./data/day_03.aoc"),
    both_parts(parse_lines(Input), 1, [], []).

solve_timed() -> util:timed(fun solve/0).