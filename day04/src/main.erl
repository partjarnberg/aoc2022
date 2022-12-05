-module(main).
-author("par.tjarnberg").
-export([start/0, solve/0]).

start() ->
  Input = read_lines_into_list("input.txt"),
  Part = os:getenv("part", "part1"),
  Worker = spawn(main, solve,[]),
  if
  Part == "part2" ->
    Worker ! {part2, Input};
  true ->
    Worker ! {part1, Input}
  end.

solve() ->
  receive
    {part1, Input} ->
      Filtered = lists:filter(fun ([_First|[_Second]]) -> find_all(_First, _Second) end, Input),
      erlang:display(length(Filtered));
    {part2, Input} ->
      Filtered = lists:filter(fun ([_First|[_Second]]) -> find_any(_First, _Second) end, Input),
      erlang:display(length(Filtered))
  end.

find_all(First, Second) ->
  FirstPartOfSecond = lists:all(fun (Elem) -> lists:member(Elem, Second) end, First),
  if
    FirstPartOfSecond -> true;
    true ->
      SecondPartOfFirst = lists:all(fun (Elem) -> lists:member(Elem, First) end, Second),
      SecondPartOfFirst
  end.

find_any(First, Second) ->
  FirstPartOfSecond = lists:any(fun (Elem) -> lists:member(Elem, Second) end, First),
  if
    FirstPartOfSecond -> true;
    true -> false
  end.

range_in_compartment(Compartment) ->
  _Temp = re:split(Compartment, "-"),
  lists:seq(binary_to_integer(lists:nth(1, _Temp)), binary_to_integer(lists:nth(2, _Temp))).

read_lines_into_list(Filename) ->
  {ok, Data} = file:read_file(Filename),
  lists:map(fun(Rucksack) -> lists:map(fun (Compartment) -> range_in_compartment(Compartment) end, Rucksack) end, lists:map(fun (Row) -> re:split(Row, ",") end, re:split(Data,"\n"))).