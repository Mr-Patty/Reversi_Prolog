permissible_move(X, Y, Player, Grid) :-
  not_edge(X, Y),
  empty_cell(X, Y, Grid),
  (left(Grid, X, Y, Player);
   left_up(Grid, X, Y, Player);
   up(Grid, X, Y, Player);
   up_right(Grid, X, Y, Player);
   right(Grid, X, Y, Player);
   right_down(Grid, X, Y, Player);
   down(Grid, X, Y, Player);
   down_left(Grid, X, Y, Player)).

empty_cell(X, Y, Grid) :-
  iter_elem(Grid, X, Y, E), E = "-".

not_edge(X,Y) :- (X < 9), (Y < 9), (X > 0), (Y > 0).
not(X):-X,!,fail.
not(_).

left(Grid, X, Y, Player) :- X2 is X - 1, Y2 is Y,
  not_edge(X2, Y2),
  not(empty_cell(X2, Y2, Grid)),
  iter_elem(Grid, X2, Y2, Res),
  opposite_player(Player, Opponent),
  Res = Opponent,
  left_check(Grid, X2, Y2, Opponent).
left_check(Grid, X, Y, Player) :-
  not_edge(X, Y),
  not(empty_cell(X, Y, Grid)),
  iter_elem(Grid, X, Y, Res),
  opposite_player(Player, Opponent),
  Res = Opponent.
left_check(Grid, X, Y, Player) :-
  not_edge(X, Y),
  not(empty_cell(X, Y, Grid)),
  iter_elem(Grid, X, Y, Res),
  Res = Player,
  X2 is X - 1, Y2 is Y,
  left_check(Grid, X2, Y2, Player).


left_up(Grid, X, Y, Player) :- X2 is X - 1, Y2 is Y - 1,
  not_edge(X2, Y2),
  not(empty_cell(X2, Y2, Grid)),
  iter_elem(Grid, X2, Y2, Res),
  opposite_player(Player, Opponent),
  Res = Opponent,
  left_up_check(Grid, X2, Y2, Opponent).
left_up_check(Grid, X, Y, Player) :-
  not_edge(X, Y),
  not(empty_cell(X, Y, Grid)),
  iter_elem(Grid, X, Y, Res),
  opposite_player(Player, Opponent),
  Res = Opponent.
left_up_check(Grid, X, Y, Player) :-
  not_edge(X, Y),
  not(empty_cell(X, Y, Grid)),
  iter_elem(Grid, X, Y, Res),
  Res = Player,
  X2 is X - 1, Y2 is Y - 1,
  left_up_check(Grid, X2, Y2, Player).


up(Grid, X, Y, Player) :- X2 is X, Y2 is Y - 1,
  not_edge(X2, Y2),
  not(empty_cell(X2, Y2, Grid)),
  iter_elem(Grid, X2, Y2, Res),
  opposite_player(Player, Opponent),
  Res = Opponent,
  up_check(Grid, X2, Y2, Opponent).
up_check(Grid, X, Y, Player) :-
  not_edge(X, Y),
  not(empty_cell(X, Y, Grid)),
  iter_elem(Grid, X, Y, Res),
  opposite_player(Player, Opponent),
  Res = Opponent.
up_check(Grid, X, Y, Player) :-
  not_edge(X, Y),
  not(empty_cell(X, Y, Grid)),
  iter_elem(Grid, X, Y, Res),
  Res = Player,
  X2 is X, Y2 is Y - 1,
  up_check(Grid, X2, Y2, Player).


up_right(Grid, X, Y, Player) :- X2 is X + 1, Y2 is Y - 1,
  not_edge(X2, Y2),
  not(empty_cell(X2, Y2, Grid)),
  iter_elem(Grid, X2, Y2, Res),
  opposite_player(Player, Opponent),
  Res = Opponent,
  up_right_check(Grid, X2, Y2, Opponent).
up_right_check(Grid, X, Y, Player) :-
  not_edge(X, Y),
  not(empty_cell(X, Y, Grid)),
  iter_elem(Grid, X, Y, Res),
  opposite_player(Player, Opponent),
  Res = Opponent.
up_right_check(Grid, X, Y, Player) :-
  not_edge(X, Y),
  not(empty_cell(X, Y, Grid)),
  iter_elem(Grid, X, Y, Res),
  Res = Player,
  X2 is X + 1, Y2 is Y - 1,
  up_right_check(Grid, X2, Y2, Player).


right(Grid, X, Y, Player) :- X2 is X + 1, Y2 is Y,
  not_edge(X2, Y2),
  not(empty_cell(X2, Y2, Grid)),
  iter_elem(Grid, X2, Y2, Res),
  opposite_player(Player, Opponent),
  Res = Opponent,
  right_check(Grid, X2, Y2, Opponent).
right_check(Grid, X, Y, Player) :-
  not_edge(X, Y),
  not(empty_cell(X, Y, Grid)),
  iter_elem(Grid, X, Y, Res),
  opposite_player(Player, Opponent),
  Res = Opponent.
right_check(Grid, X, Y, Player) :-
  not_edge(X, Y),
  not(empty_cell(X, Y, Grid)),
  iter_elem(Grid, X, Y, Res),
  Res = Player,
  X2 is X + 1, Y2 is Y,
  right_check(Grid, X2, Y2, Player).


right_down(Grid, X, Y, Player) :- X2 is X + 1, Y2 is Y + 1,
  not_edge(X2, Y2),
  not(empty_cell(X2, Y2, Grid)),
  iter_elem(Grid, X2, Y2, Res),
  opposite_player(Player, Opponent),
  Res = Opponent,
  right_down_check(Grid, X2, Y2, Opponent).
right_down_check(Grid, X, Y, Player) :-
  not_edge(X, Y),
  not(empty_cell(X, Y, Grid)),
  iter_elem(Grid, X, Y, Res),
  opposite_player(Player, Opponent),
  Res = Opponent.
right_down_check(Grid, X, Y, Player) :-
  not_edge(X, Y),
  not(empty_cell(X, Y, Grid)),
  iter_elem(Grid, X, Y, Res),
  Res = Player,
  X2 is X + 1, Y2 is Y + 1,
  right_down_check(Grid, X2, Y2, Player).


down(Grid, X, Y, Player) :- X2 is X, Y2 is Y + 1,
  not_edge(X2, Y2),
  not(empty_cell(X2, Y2, Grid)),
  iter_elem(Grid, X2, Y2, Res),
  opposite_player(Player, Opponent),
  Res = Opponent,
  down_check(Grid, X2, Y2, Opponent).
down_check(Grid, X, Y, Player) :-
  not_edge(X, Y),
  not(empty_cell(X, Y, Grid)),
  iter_elem(Grid, X, Y, Res),
  opposite_player(Player, Opponent),
  Res = Opponent.
down_check(Grid, X, Y, Player) :-
  not_edge(X, Y),
  not(empty_cell(X, Y, Grid)),
  iter_elem(Grid, X, Y, Res),
  Res = Player,
  X2 is X, Y2 is Y + 1,
  down_check(Grid, X2, Y2, Player).

down_left(Grid, X, Y, Player) :- X2 is X - 1, Y2 is Y + 1,
  not_edge(X2, Y2),
  not(empty_cell(X2, Y2, Grid)),
  iter_elem(Grid, X2, Y2, Res),
  opposite_player(Player, Opponent),
  Res = Opponent,
  down_left_check(Grid, X2, Y2, Opponent).
down_left_check(Grid, X, Y, Player) :-
  not_edge(X, Y),
  not(empty_cell(X, Y, Grid)),
  iter_elem(Grid, X, Y, Res),
  opposite_player(Player, Opponent),
  Res = Opponent.
down_left_check(Grid, X, Y, Player) :-
  not_edge(X, Y),
  not(empty_cell(X, Y, Grid)),
  iter_elem(Grid, X, Y, Res),
  Res = Player,
  X2 is X - 1, Y2 is Y + 1,
  down_left_check(Grid, X2, Y2, Player).
