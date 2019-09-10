%% Main boot point of the program.
%%
%% @author: Michael Bausano <bausanomichal@gmail.com>
-module(main).

%% Boot function has to be always exported with zero arity.
-export([start/0]).

-import(machine, [new/1, r/1, l/1, p/2, e/1]).

%% This is where it all starts.
start() ->
  Machine = machine:new([
    s, none, e,
    1, none, 0, none, 1, none, 1, none, 1, none, 1, none, 1, none, 1, none, 1, none, 0, none, 1, none, %% 1533
    none, e,
    1, none, 1, none, 1, none, 1, none, 1, none, 0, none, 1, none, 1, none, 1, none, 0, none, 0, none, %% 479
    none, r
  ]),
  {_, Tape} = start(Machine),
  io:format("~p\n", [Tape]).

start({t, Machine}) -> next(r(r(p(e, e(Machine)))));
start({s, Machine}) -> next(r(r(Machine)));
start({1, Machine}) -> start(l(Machine));
start({0, Machine}) -> start(l(Machine));
start({_, Machine}) -> start(l(l(Machine))).

next({x, Machine}) -> next(r(r(Machine)));
next({e, Machine}) -> next(r(r(Machine)));
next({none, Machine}) -> decide(l(p(x, Machine)));
next({r, Machine}) -> finish({r, Machine}).

decide({1, Machine}) -> move_carry(r(Machine));
decide({0, Machine}) -> move_clean(r(Machine)).

move_carry({r, Machine}) -> place_carry(r(r(Machine)));
move_carry({e, Machine}) -> move_carry(r(r(p(t, e(Machine)))));
move_carry({_, Machine}) -> move_carry(r(r(Machine))).

move_clean({r, Machine}) -> place_clean(r(r(Machine)));
move_clean({e, Machine}) -> move_clean(r(r(p(t, e(Machine)))));
move_clean({_, Machine}) -> move_clean(r(r(Machine))).

place_carry({x, Machine}) -> place_carry(r(r(Machine)));
place_carry({none, Machine}) -> start(p(c, Machine));
place_carry({i, Machine}) -> start(p(1, l(p(x, e(Machine)))));
place_carry({d, Machine}) -> start(p(g, e(Machine)));
place_carry({g, Machine}) -> start(p(d, r(r(r(p(1, l(p(x, e(Machine)))))))));
place_carry({c, Machine}) -> start(p(d, r(r(r(p(0, l(p(x, e(Machine))))))))).

place_clean({x, Machine}) -> place_clean(r(r(Machine)));
place_clean({none, Machine}) -> start(p(i, Machine));
place_clean({i, Machine}) -> start(p(0, l(p(x, e(Machine)))));
place_clean({d, Machine}) -> start(p(c, e(Machine)));
place_clean({c, Machine}) -> start(p(1, l(p(x, e(Machine)))));
place_clean({g, Machine}) -> start(p(d, r(r(r(p(0, l(p(x, e(Machine))))))))).

finish({d, Machine}) -> finish(p(1, l(e(Machine))));
finish({g, Machine}) -> finish(p(1, l(e(Machine))));
finish({i, Machine}) -> finish(p(0, l(e(Machine))));
finish({none, Machine}) -> Machine;
finish({_, Machine}) -> finish(r(r(Machine))).
