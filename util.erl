-module(util).
-export([
    timed/1, print/1, print/2, 
    c/0, c/1, c/2, r/0, r/1, rt/0, rt/1, cr/0, cr/1, crt/0, crt/1, 
    bm/0, bm/1, bmd/1, bmd/2
]).

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
compile_modules(Files, debug_info) -> compile_modules(Files, [], debug_info);
compile_modules([], Compiled) -> {ok,lists:sort(Compiled)};
compile_modules([FileName|FileNames], Compiled) -> 
        compile:file(FileName),
        CurrentCompiled = [list_to_atom(lists:sublist(FileName, length(FileName) - 4))|Compiled],
        compile_modules(FileNames, CurrentCompiled).
compile_modules([], Compiled, debug_info) -> {ok,lists:sort(Compiled), debug_info};
compile_modules([FileName|FileNames], Compiled, debug_info) -> 
        compile:file(FileName, debug_info),
        CurrentCompiled = [list_to_atom(lists:sublist(FileName, length(FileName) - 4))|Compiled],
        compile_modules(FileNames, CurrentCompiled, debug_info).

c() -> 
    Files = find_erl_files(),
    compile_modules(Files).
c(debug_info) ->
    Files = find_erl_files(),
    compile_modules(Files, debug_info);
c(DayId) -> compile:file("day_"++int_to_id(DayId)++".erl", debug_info).
c(DayId, debug_info) -> compile:file("day_"++int_to_id(DayId)++".erl", debug_info).

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

%Benchmark

bm() -> bm(1000).
bm(X) -> 
    Files = lists:sort(fun day_precedes/2, lists:filter(fun is_day/1, find_erl_files())),
    Modules = [list_to_atom(lists:flatten(rm_ss(FileName, ".erl"))) || FileName <- Files],
    benchmark_all(Modules, X).

bmd(D) -> bmd(D, 1000).
bmd(D, X) ->
    Module = list_to_atom("day_" ++ int_to_id(D)),
    benchmark(Module, X).

benchmark_all(Modules, X) -> benchmark_all(Modules, X, []).
benchmark_all([], _X, Results) -> maps:from_list(Results);
benchmark_all([Module|Modules], X, Results) -> 
    Exports = maps:from_list(Module:module_info(exports)),
    Exists = maps:is_key(solve, Exports) and (maps:get(solve, Exports) == 0),
    Result = if
        Exists -> {Module, benchmark(Module, X)};
        true -> {Module, #{function => solve, result => not_found}}
    end,
    benchmark_all(Modules, X, [Result|Results]).

benchmark(M, X) ->
    ResultXTimes = timed(fun() -> run_x(M, X) end),
    AvgMicroSecs = round(maps:get(microseconds, ResultXTimes) / X),
    Result = maps:get(result, ResultXTimes),
    #{  
        runs => X,
        avg_microsecs => AvgMicroSecs,
        result => Result
    }.

run_x(M, X) when X == 1 -> M:solve();
run_x(M, X) -> 
    M:solve(),
    run_x(M, X-1).

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