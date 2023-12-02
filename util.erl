-module(util).
-export([timed/1, c/0, c/1]).

timed(F) ->
    Start = erlang:system_time(microsecond),
    Result = F(),
    End = erlang:system_time(microsecond),
    {{result, Result}, {microsecs, End-Start}}.

find_erl_files() ->
    {ok, Files} = file:list_dir("."),
    [File || File <- Files, check_erl(File)].

check_erl(File_name) -> lists:sublist(lists:reverse(File_name), 4) == "lre.".

compile_modules(Files) -> compile_modules(Files, []).
compile_modules([], Compiled) -> {ok,lists:sort(Compiled)};
compile_modules([FileName|FileNames], Compiled) -> 
    if
        FileName == "cmp.erl" -> 
            compile_modules(FileNames, Compiled);
        true -> 
            compile:file(FileName),
            CurrentCompiled = [list_to_atom(lists:sublist(FileName, length(FileName)-4))|Compiled],
            compile_modules(FileNames, CurrentCompiled)
    end.

c() -> 
    Files = find_erl_files(),
    compile_modules(Files).
c(Day) -> compile:file("day"++integer_to_list(Day)++".erl").