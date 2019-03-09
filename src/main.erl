%% Main boot point of the program.
%%
%% @author:
-module(main).

%% Boot function has to be always exported with zero arity.
-export([start/0]).

-import(machine, [new/1, r/1, l/1, p/2, e/1]).

%% This is where it all starts.
start() ->
  Machine = machine:new([s, none, e, 0, none, 0, none, 0, none, 0, none, none, e, 1, none, 1, none, 0, none, 1, none, none, r]),
  io:format("~p\n", [start(Machine)]).

turing1({ _, N, _ } = Machine) when N > 20 -> Machine;
turing1({ none, _, _} = Machine) -> turing1(p(0, Machine));
turing1({ 0, _, _ } = Machine) -> turing1(p(1, r(r(Machine))));
turing1({ 1, _, _ } = Machine) -> turing1(p(0, r(r(Machine)))).

start({ s, _, _} = Machine) -> next(r(r(Machine)));
start({ 1, _, _} = Machine) -> start(l(Machine));
start({ 0, _, _} = Machine) -> start(l(Machine));
start(Machine) -> start(l(l(Machine))).

next({ x, _, _} = Machine) -> next(r(r(Machine)));
next({ e, _, _} = Machine) -> next(r(r(Machine)));
next({ none, _, _ } = Machine) -> decide(l(p(x, Machine)));
next({ r, _, _} = Machine) -> place_clean(r(r(p(f, e(Machine)))));
next({ f, _, _ } = Machine) -> Machine.

decide({ 1, _, _ } = Machine) -> move_carry(r(Machine));
decide({ 0, _, _ } = Machine) -> move_clean(r(Machine)).

move_carry({ r, _, _ } = Machine) -> place_carry(r(r(Machine)));
move_carry(Machine) -> move_carry(r(r(Machine))).

move_clean({ r, _, _ } = Machine) -> place_clean(r(r(Machine)));
move_clean(Machine) -> move_clean(r(r(Machine))).

place_carry({ x, _, _} = Machine) -> place_carry(r(r(Machine)));
place_carry({ none, _, _} = Machine) -> start(p(c, Machine));
place_carry({ i, _, _} = Machine) -> start(p(1, l(p(x, e(Machine)))));
place_carry({ d, _, _} = Machine) -> start(p(g, e(Machine)));
place_carry({ g, _, _} = Machine) -> start(p(d, r(r(r(p(1, l(p(x, e(Machine)))))))));
place_carry({ c, _, _} = Machine) -> start(p(d, r(r(r(p(0, l(p(x, e(Machine))))))))).

place_clean({ x, _, _} = Machine) -> place_clean(r(r(Machine)));
place_clean({ none, _, _} = Machine) -> start(p(i, Machine));
place_clean({ i, _, _} = Machine) -> start(p(0, l(p(x, e(Machine)))));
place_clean({ d, _, _} = Machine) -> start(p(g, e(Machine)));
place_clean(Machine) -> start(p(1, l(p(x, e(Machine))))).