%% Exports instruction set for Turing's a-machine.
%%
%% @author: Michael Bausano <bausanomichal@gmail.com>
-module(machine).

-export([new/1, r/1, l/1, p/2, e/1]).

new([Start | _] = Tape) -> { Start, 0, Tape }.

r({ _, Pointer, Tape }) -> {
    nth(Pointer + 1, Tape),
    Pointer + 1, Tape
}.

l({ _, Pointer, Tape }) when Pointer > 0 -> {
    nth(Pointer - 1, Tape),
    Pointer - 1, Tape
}.

p(Symbol, {_, Pointer, Tape}) -> {
    Symbol,
    Pointer,
    set(Pointer, Tape, Symbol)
}.

e(Machine) -> p(none, Machine).

nth(N, List) -> nth(N, List, 0).
nth(N, [Element | _], N) -> Element;
nth(_, [], _) -> none;
nth(N, [_ | Tail], M) -> nth(N, Tail, M + 1).

set(Pointer, Tape, Symbol) -> set(0, Pointer, Tape, Symbol).
set(N, N, [_ | Tail], Symbol) -> [Symbol | Tail];
set(N, Pointer, [Head | Tail], Symbol) ->
    [Head | set(N + 1, Pointer, Tail, Symbol)];
set(N, N, [], Symbol) -> [Symbol];
set(N, Pointer, [], Symbol) -> [none | set(N + 1, Pointer, [], Symbol)].