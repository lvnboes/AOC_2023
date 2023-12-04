-module(util).
-export([timed/1, print/1, print/2, c/0, c/1, r/0, r/1, r/2, rt/0, rt/1, cr/0, cr/1, crt/0, crt/1]).

%Timer

timed(F) ->
    Start = erlang:system_time(microsecond),
    Result = F(),
    End = erlang:system_time(microsecond),
    #{result => Result, microseconds => End-Start}.

%Print for debugging purposes

print(Input) -> print(Input, 1).
print(Input, NewLines) ->
    FormatPattern = lists:flatten(["~p"|["~n" || _ <- lists:seq(1,NewLines)]]),
    io:format(FormatPattern, [Input]).

%Compile all modules

compile_modules(Files) -> compile_modules(Files, []).
compile_modules([], Compiled) -> {ok,lists:sort(Compiled)};
compile_modules([FileName|FileNames], Compiled) -> 
    if
        FileName == "util.erl" -> 
            compile_modules(FileNames, Compiled);
        true -> 
            compile:file(FileName),
            CurrentCompiled = [list_to_atom(lists:sublist(FileName, length(FileName) - 4))|Compiled],
            compile_modules(FileNames, CurrentCompiled)
    end.

c() -> 
    Files = find_erl_files(),
    compile_modules(Files).
c(DayId) -> compile:file("day_"++int_to_id(DayId)++".erl").

%Run all days

is_day(FileName) -> 
    lists:sublist(string:lowercase(FileName), 1, 4) == "day_" 
    andalso 
    lists:sublist(string:lowercase(FileName), length(FileName) - 3, length(FileName)) == ".erl".

get_file_number(FileName) -> 
    StrippedName = rm_ss(string:lowercase(FileName), ["day_", ".erl"]),
    try list_to_integer(StrippedName) of Val -> Val
    catch error:_ -> 99
    end.

day_precedes(A, B) -> get_file_number(A) >= get_file_number(B).

run(Modules, Function) -> run(Modules, Function, []).
run([], _Function, Results) -> maps:from_list(Results);
run([Module|Modules], Function, Results) -> 
    Exports = maps:from_list(Module:module_info(exports)),
    Exists = maps:is_key(Function, Exports) and (maps:get(Function, Exports) == 0),
    Result = if
        Exists -> {Module, Module:Function()};
        true -> {Module, #{function => Function, result => not_found}}
    end,
    run(Modules, Function, [Result|Results]).


r() -> r(solve).
r(DayId) when is_integer(DayId) -> r(DayId, solve);
r(timed) -> r(solve_timed);
r(Function) ->
    Files = lists:sort(fun day_precedes/2, lists:filter(fun is_day/1, find_erl_files())),
    Modules = [list_to_atom(lists:flatten(rm_ss(FileName, ".erl"))) || FileName <- Files],
    run(Modules, Function).
r(DayId, timed) -> r(DayId, solve_timed);
r(DayId, Function) when is_integer(DayId) -> 
    Module = list_to_atom("day_"++int_to_id(DayId)),
    run([Module], Function).

rt() -> r(timed).
rt(DayId) -> r(DayId, timed).

cr() -> {c(), r()}.
cr(DayId) -> {c(DayId), r(DayId)}.

crt() -> {c(), rt()}.
crt(DayId) -> {c(DayId), rt(DayId)}.

%Common

find_erl_files() ->
    {ok, Files} = file:list_dir("."),
    [File || File <- Files, check_erl(File)].

check_erl(File_name) -> lists:sublist(lists:reverse(File_name), 4) == "lre.".


rm_ss(String, []) -> String;
rm_ss(String, Remove) ->
    RemoveIsString = io_lib:char_list(Remove),
    if 
        RemoveIsString -> lists:flatten(string:replace(String, Remove, ""));
        true ->
            [H|T] = Remove,
            rm_ss(lists:flatten(string:replace(String, H, "")), T)
    end.

int_to_id(Int) -> lists:flatten(string:pad(integer_to_list(Int), 2, leading, $0)).