-module(day_08).
-export([solve/0, solve_timed/0]).

%Reading and parsing input

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
    {binary_to_list(Node), list_to_tuple([binary_to_list(PathOption) || PathOption <- binary:split(LR, <<", ">>)])}.

%Part 2

get_part_2_start_nodes(NodesMap) ->
    lists:filter(
        fun(N) -> lists:nth(3, N) == $A end, 
        [Node || {Node, _Directions} <- maps:to_list(NodesMap)]
    ).

find_finishes(Starts, FinishCond, Instructions, NodesMap) -> 
    find_finishes(Starts, FinishCond, Instructions, NodesMap, []).
find_finishes([], _FinishCond, _Instructions, _NodesMap, Finishes) ->
    Finishes;
find_finishes([Start | Starts], FinishCond, Instructions, NodesMap, Finishes) ->
    find_finishes(
        Starts, FinishCond, Instructions, NodesMap, 
        [execute_instructions(Start, FinishCond, Instructions, NodesMap) | Finishes]
    ).

calc_lcm([A]) -> A;
calc_lcm([A, B | T]) -> calc_lcm([lcm(A, B) | T]).

gcd(A,B) when A == B -> A;
gcd(A,B) when A > B -> gcd(A-B, B);
gcd(A,B) -> gcd(A, B-A).

lcm(A,B) -> (A*B) div gcd(A, B).

%Common

execute_instructions(Node, FinishCond, Instructions, Map) -> 
    execute_instructions(Node, FinishCond, Instructions, [], Map, 0).

execute_instructions(Node, FinishCond, [Instruction|T], Executed, Map, Count) ->
    case FinishCond(Node) of
        true -> Count;
        false ->
            if 
                length(T) == 0 -> 
                    Instructions = lists:reverse([Instruction|Executed]),
                    ExecInstr = [];
                true -> 
                    Instructions = T,
                    ExecInstr = [Instruction | Executed]
            end,
            case Instruction of
                $L -> {N, _R} = maps:get(Node, Map);
                $R -> {_L, N} = maps:get(Node, Map)
            end,
        execute_instructions(N, FinishCond, Instructions, ExecInstr, Map, Count+1)
    end.

solve() -> 
    {Instructions, NodesMap} = process_imports("./data/day_08.aoc"),
    Part1 = execute_instructions("AAA", fun(X) -> X == "ZZZ" end, Instructions, NodesMap),
    StartNodes2 = get_part_2_start_nodes(NodesMap),
    Finishes = find_finishes(StartNodes2, fun(X) -> lists:nth(3, X) == $Z end, Instructions, NodesMap),
    Part2 = calc_lcm(Finishes),
    #{part1 => Part1, part2 => Part2}.

solve_timed() -> util:timed(fun solve/0).