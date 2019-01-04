:- use_module(library(clpfd)).

% Instead of working directly with columns, we work with the rows of a
% transposed grid.

% Single step for a row. Last two lines ensure it fails if the list contains
% anything other than 'x' and 'o'
row_step([x,x,o|T], [o,o,x|T]).
row_step([o,x,x|T], [x,o,o|T]).
row_step([H|T1],[H|T2]) :- row_step(T1, T2).


% Computes a single jump on the rows without transposing the board.
% if they have the same first row you can't move a peg on that row:
no_flip_jump([R|B1], [R|B2]) :- no_flip_jump(B1, B2).
% if they have a different first row, can first row be stepped to the second row.
no_flip_jump([R1|B], [R2|B]) :- row_step(R1, R2).

% Checks if B1 or its transpose can be jumped respectively to B2 or its
% transpose. The reason we have a 'no_flip' version is so that it doesn't just
% keep transposing it over and over looking for a solution that doesn't exist.
jump(B1, B2) :- no_flip_jump(B1, B2).
jump(B1, B2) :- transpose(B1, FlipB1), no_flip_jump(FlipB1, FlipB2), transpose(FlipB2, B2).

% determines if a list has only one 'X'
unique(X, [Y|Ys]) :- X \= Y, unique(X, Ys).
unique(X, [X|Ys]) :- \+ member(X, Ys).

% checks if a board is solved (i.e. that there's only one 'x')
solved(B) :- flatten(B, Flat), unique(x, Flat).

jumps([B]) :- solved(B).
jumps([B1,B2|Bs]) :- jump(B1, B2), jumps([B2|Bs]).

% Solved board
%[x,o,o,o,o]       [o,o,o,o,o]      [o,o,o,o,o]
%[x,o,o,o,o] ====  [o,o,o,o,o] ==== [x,o,o,o,o]
%[o,o,o,o,o] ====  [x,o,o,o,o] ==== [o,o,o,o,o]
%[x,o,o,o,o]       [x,o,o,o,o]      [o,o,o,o,o]
%[o,o,o,o,o]       [o,o,o,o,o]      [o,o,o,o,o]
