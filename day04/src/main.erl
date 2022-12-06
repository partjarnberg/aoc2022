-module(main).
-author("par.tjarnberg").
-export([start/0, solve/0]).

start() ->
  Input = parse_input("input.txt"),
  Worker = spawn(main, solve,[]),
  case os:getenv("part", "part1") of
    "part1" -> Worker ! {part1, Input};
    "part2" -> Worker ! {part2, Input}
  end.

solve() ->
  receive
    {part1, Input} -> erlang:display(length(lists:filter(fun ([First_elf |[Second_elf]]) -> find_all(First_elf, Second_elf) end, Input)));
    {part2, Input} -> erlang:display(length(lists:filter(fun ([First_elf |[Second_elf]]) -> find_any(First_elf, Second_elf) end, Input)))
  end.

find_all(First_elf_sections, Second_elf_Sections) ->
  case lists:all(fun (Section) -> lists:member(Section, Second_elf_Sections) end, First_elf_sections) of
    true -> true;
    false -> lists:all(fun (Elem) -> lists:member(Elem, First_elf_sections) end, Second_elf_Sections)
  end.

find_any(First_elf_sections, Second_elf_sections) ->
  lists:any(fun (Elem) -> lists:member(Elem, Second_elf_sections) end, First_elf_sections).

list_of_sections_for_elf(Elf) ->
  [From, To] = re:split(Elf, "-"),
  lists:seq(binary_to_integer(From), binary_to_integer(To)).

parse_input(Filename) ->
  {ok, Data} = file:read_file(Filename),
  lists:map(fun(Pair_of_elves) ->
    lists:map(fun (Elf) -> list_of_sections_for_elf(Elf) end, Pair_of_elves) end,
    lists:map(fun (Row) -> re:split(Row, ",") end, re:split(Data,"\n"))).