-module(main).
-author("par.tjarnberg").
-export([start/0, solve/0]).

start() ->
  Input = read_lines_into_list("input.txt"),
  Worker = spawn(main, solve,[]),
  case os:getenv("part", "part1") of
    "part1" -> Worker ! {part1, Input};
    "part2" -> Worker ! {part2, Input}
  end.

solve() ->
  receive
    {part1, Input} -> erlang:display(length(lists:filter(fun ([Head|[Tail]]) -> find_all(Head, Tail) end, Input)));
    {part2, Input} -> erlang:display(length(lists:filter(fun ([Head|[Tail]]) -> find_any(Head, Tail) end, Input)))
  end.

find_all(First, Second) ->
  case lists:all(fun (Elem) -> lists:member(Elem, Second) end, First) of
    true -> true;
    false -> lists:all(fun (Elem) -> lists:member(Elem, First) end, Second)
  end.

find_any(First, Second) ->
  lists:any(fun (Elem) -> lists:member(Elem, Second) end, First).

range_in_compartment(Compartment) ->
  [Head, Tail] = re:split(Compartment, "-"),
  lists:seq(binary_to_integer(Head), binary_to_integer(Tail)).

read_lines_into_list(Filename) ->
  {ok, Data} = file:read_file(Filename),
  lists:map(fun(Rucksack) ->
    lists:map(fun (Compartment) -> range_in_compartment(Compartment) end, Rucksack) end,
    lists:map(fun (Row) -> re:split(Row, ",") end, re:split(Data,"\n"))).