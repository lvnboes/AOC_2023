-module(day_08).
-export([solve/0, solve_timed/0]).

process_imports(Path) ->
    {ok, File} = file:read_file(Path),
    [Instructions, Nodes] = binary:split(File, <<"\n\n">>),
    ParsedNodes = [
        parse_line(binary:split(Line, <<" = ">>)) 
        || Line <- binary:split(
            binary:replace(
                binary:replace(
                    Nodes, <<"(">>, <<"">>, [global]
                ), <<")">>, <<"">>, [global]
            ), <<"\n">>, [global]
        )
    ],
    {binary_to_list(Instructions), maps:from_list(ParsedNodes)}.

parse_line([Node, LR]) -> 
    {Node, list_to_tuple(binary:split(LR, <<", ">>))}.

execute_instructions(Node, Instructions, Map) -> 
    execute_instructions(Node, Instructions, [], Map, 0).
execute_instructions(<<"ZZZ">>, _Instructions, _Executed, _Map, Count) -> 
    Count;
execute_instructions(Node, [Instruction], Executed, Map, Count) 
  when Instruction == $L -> 
    {L, _R} = maps:get(Node, Map),
    Instructions = lists:reverse([Instruction|Executed]),
    execute_instructions(L, Instructions, [], Map, Count+1);
execute_instructions(Node, [Instruction | T], Executed, Map, Count) 
  when Instruction == $L -> 
    {L, _R} = maps:get(Node, Map),
    execute_instructions(L, T, [Instruction | Executed], Map, Count+1);
execute_instructions(Node, [Instruction], Executed, Map, Count) 
  when Instruction == $R -> 
    {_L, R} = maps:get(Node, Map),
    Instructions = lists:reverse([Instruction|Executed]),
    execute_instructions(R, Instructions, [], Map, Count+1);
execute_instructions(Node, [Instruction|T], Executed, Map, Count) 
  when Instruction == $R -> 
    {_L, R} = maps:get(Node, Map),
    execute_instructions(R, T, [Instruction | Executed], Map, Count+1).

solve() -> 
    {Instructions, NodesMap} = process_imports("./data/day_08.aoc"),
    execute_instructions(<<"AAA">>, Instructions, NodesMap).

solve_timed() -> util:timed(fun solve/0).