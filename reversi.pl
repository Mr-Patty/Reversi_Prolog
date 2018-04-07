:- include('./reversi-display.pl').
:- include('./reversi-rules.pl').

:- use_module(library(random)).
use_module(library(pce)).
start :- new(DW, dialog('Начальные настройки')),
         send_list(DW,append,
            [new(_, text('Set the Players\n 1 - Human\n 2 - Computer')),
            new(PlayerBlack, int_item(player_Black, low := 1,
                   high := 2, default := 1)),
            new(PlayerWhite,int_item(player_White, low := 1,
                  high := 2, default := 1)),
            new(Depth, int_item(depth, low := 1, high := 6, default := 1))] ),
         send_list(DW,append,
               [button('Начать',
                        block(message(@prolog, next,
                        PlayerBlack?selection,
                        PlayerWhite?selection,
                        Depth?selection), message(DW, destroy)))]),
       send(DW, append, button(exit, and(message(DW, destroy)) ) ),
                     send(DW, open).

next(PlayerBlack, PlayerWhite, Depth) :-
  new(DW2, dialog('Реверси')),
  new(Picture, picture),
  send(Picture, width(325)),
  send(Picture, height(325)),
  send_list(DW2, append,
                [Picture,
                new(X, int_item(x, low := 1, high := 8, default := 1)),
                new(Y, int_item(y, low := 1, high := 8, default := 1))]),
  setGrid(Grid),
  % listOfMoves(Grid, black, List),
  display_board(Picture, Grid),
  assert(board(Grid)),
  assert(player(black, PlayerBlack)),
  assert(player(white, PlayerWhite)),
  assert(current_player(black)),
  assert(depth(Depth)),
  send(DW2, append, button('Move', message(@prolog,
                                          play,
                                          Picture,
                                          DW2,
                                          X?selection,
                                          Y?selection ) )),
  send(DW2, append, button(exit,  message(@prolog,
                                          my_exit,
                                          DW2,
                                          Picture)) ),
  send(Picture, open),
  send(DW2, open).

my_exit(DW, Picture) :-
  my_retract(board(_)),
  my_retract(current_player(_)),
  my_retract(player(black, _)),
  my_retract(player(white, _)),
  my_retract(depth(_)),
  send(Picture, destroy),
  send(DW, destroy).

my_retract(X) :- retract(X), !.
my_retract(_).

% -----------------------------------------------%
play(Picture, DW, X, Y) :- current_player(P), player(P, 1), board(Grid), game(Picture, DW, P, Grid, X, Y).
play(Picture, DW, _, _) :- current_player(P), player(P, 2), board(Grid), ai(Picture, DW, P, Grid).

ai(Picture, DW, Player, Grid) :-
  opposite_player(Player, Opponent),
  depth(Depth),
  maxmin(Grid, Player, Opponent, Depth, [GridNew | _]),
  opposite_player(Player, New),
  send(Picture, clear),!,
  display_board(Picture, GridNew),
  is_finished(Picture, GridNew),
  retract(board(_)),
  assert(board(GridNew)),
  retract(current_player(_)),
  assert(current_player(New)),
  player(New, 2),
  ai(Picture, DW, New, GridNew).

game(Picture, DW, Player, Grid, X, Y) :-
  make_move(X, Y, Player, Grid, GridNew),
  opposite_player(Player, New),
  send(Picture, clear),!,
  display_board(Picture, GridNew),
  % is_finished(Picture, GridNew),
  retract(board(_)),
  assert(board(GridNew)),
  retract(current_player(_)),
  assert(current_player(New)),
  player(New, 2),
  ai(Picture, DW, New, GridNew).

is_finished(_,  Grid) :- listOfMoves(Grid, black, R1), listOfMoves(Grid, white, R2), not((R2 = [], R1 = [])), !.
is_finished(Picture,  Grid) :- playerPoint(Grid, black, Bl), playerPoint(Grid, white, Wh), Bl > Wh, send(Picture, clear), send(Picture, display, new(_, text("Black's victory"))), !.
is_finished(Picture,  Grid) :- playerPoint(Grid, black, Bl), playerPoint(Grid, white, Wh), Bl < Wh, send(Picture, clear), send(Picture, display, new(_, text("White's victory"))), !.
is_finished(Picture,  _) :- send(Picture, clear), send(Picture, display, new(_, text("Draw!"))), !.

listOfMoves(Grid, Player, Res) :- listMoves(1, 1, Grid, Player, Res).
listMoves(8, 8, Grid, Player, [8, 8]) :-
  permissible_move(8, 8, Player, Grid), !.
listMoves(8, 8, _, _, []).
listMoves(X, 8, Grid, Player, [[X, 8] | Res]) :-
  permissible_move(X, 8, Player, Grid),
  X2 is X + 1, !,
  listMoves(X2, 1, Grid, Player, Res).
listMoves(X, 8, Grid, Player, Res) :-
  X2 is X + 1, !,
  listMoves(X2, 1, Grid, Player, Res).
listMoves(X, Y, Grid, Player, [[X | Y] | Res]) :-
  permissible_move(X, Y, Player, Grid),
  Y2 is Y + 1, !,
  listMoves(X, Y2, Grid, Player, Res).
listMoves(X, Y, Grid, Player, Res) :-
  Y2 is Y + 1, !,
  listMoves(X, Y2, Grid, Player, Res).


%--------------------------------------------------------------------------------%
minmax(Grid, PlayerMax, PlayerMin, 1, [Grid | Estimate]) :-
  heuristic(Grid, PlayerMax, PlayerMin, Estimate).

minmax(Grid, PlayerMax, PlayerMin, Depth, [GridNew | Estimate]) :-
  listOfMoves(Grid, PlayerMin, List),
  % List /= [], !,
  gen_move(Grid, PlayerMin, List, GridList),
  DepthNew is Depth - 1,
  min_list(GridList, PlayerMax, PlayerMin, DepthNew, [GridNew | Estimate]).
minmax(Grid, PlayerMax, PlayerMin, _, [Grid | Estimate]) :-
  heuristic(Grid, PlayerMax, PlayerMin, Estimate).

min_list([], _, _, _, []).
min_list(GridList, PlayerMax, PlayerMin, Depth, [GridNew | Estimate]) :-
  min_list_heur(GridList, PlayerMax, PlayerMin, Depth, List_heur),
  min_of_heur(List_heur, List_min_heur),
  random_heur(List_min_heur, [GridNew | Estimate]).

min_of_heur(List_heur, Grid_Estimate) :- min_of_heur_list(List_heur, Grid_Estimate, [[_ | +inf]]).
min_of_heur_list([], Res, Res).
min_of_heur_list([[G | E] | T], Res, [[G1 | E1] | Gs]) :-
  E = E1, min_of_heur_list(T, Res, [[G | E] | [G1 | E1] | Gs]).
min_of_heur_list([[G | E] | T], Res, [[_ | E1] | _]) :-
  E < E1, max_of_heur_list(T, Res, [[G | E]]).
min_of_heur_list([_ | T], Res, [G1 | E1]) :-
  min_of_heur_list(T, Res, [G1 | E1]).

min_list_heur([], _, _, _, []).
min_list_heur([Grid | T], PlayerMax, PlayerMin, Depth, [[Grid | Estimate] | Res]) :-
  maxmin(Grid, PlayerMax, PlayerMin, Depth, [_ | Estimate]),
  min_list_heur(T, PlayerMax, PlayerMin, Depth, Res).


%------------
maxmin(Grid, PlayerMax, PlayerMin, 1, [Grid | Estimate]) :-
  heuristic(Grid, PlayerMax, PlayerMin, Estimate).
maxmin(Grid, PlayerMax, PlayerMin, Depth, [GridNew | Estimate]) :-
  listOfMoves(Grid, PlayerMax, List),
  % List /= [], !,
  % print(List),
  gen_move(Grid, PlayerMax, List, GridList),
  DepthNew is Depth - 1,
  max_list(GridList, PlayerMax, PlayerMin, DepthNew, [GridNew | Estimate]).
maxmin(Grid, PlayerMax, PlayerMin, _, [Grid | Estimate]) :-
  heuristic(Grid, PlayerMax, PlayerMin, Estimate).

max_list([], _, _, _, []).
max_list(GridList, PlayerMax, PlayerMin, Depth, [GridNew | Estimate]) :-
  max_list_heur(GridList, PlayerMax, PlayerMin, Depth, List_heur),
  max_of_heur(List_heur, List_max_heur),
  random_heur(List_max_heur, [GridNew | Estimate]).

random_heur(List, Elem) :-
  length(List, N),
  random_between(0, N, R),
  nth0(R, List, Elem).

max_of_heur(List_heur, Grid_Estimate) :- max_of_heur_list(List_heur, Grid_Estimate, [[_ | -inf]]).
max_of_heur_list([], Res, Res).
max_of_heur_list([[G | E] | T], Res, [[G1 | E1] | Gs]) :-
  E = E1, max_of_heur_list(T, Res, [[G | E] | [G1 | E1] | Gs]).
max_of_heur_list([[G | E] | T], Res, [[_ | E1] | _]) :-
  E > E1, max_of_heur_list(T, Res, [[G | E]]).
max_of_heur_list([_ | T], Res, [G1 | E1]) :-
  max_of_heur_list(T, Res, [G1 | E1]).

max_list_heur([], _, _, _, []).
max_list_heur([Grid | T], PlayerMax, PlayerMin, Depth, [[Grid | Estimate] | Res]) :-
  minmax(Grid, PlayerMax, PlayerMin, Depth, [_ | Estimate]),
  max_list_heur(T, PlayerMax, PlayerMin, Depth, Res).


gen_move(_, _, [], []).
gen_move(Grid, Player, [[X | Y] | Ts], [GridNew | Res]) :-
  move(X, Y, Player, Grid, GridNew),
  gen_move(Grid, Player, Ts, Res).

max_heurisic(Grids, Player, Opponent, Grid) :-
  max_heur(Grids, Player, Opponent, Grid, [], -inf).

max_heur([], _, _, Res, Res, _).
max_heur([Grid | Ts], Player, Opponent, Res, _, MaxValue) :-
  heuristic(Grid, Player, Opponent, Value),
  Value > MaxValue,
  max_heur(Ts, Player, Opponent, Res, Grid, Value).
max_heur([_ | Ts], Player, Opponent, Res, ResTmp, MaxValue) :-
  max_heur(Ts, Player, Opponent, Res, ResTmp, MaxValue).

playerPoint([], _, 0).
playerPoint([Es | Ts], Player, Res) :-
  point(Es, Player, R1),
  playerPoint(Ts, Player, R2),
  Res is R1 + R2.

point([], _, 0).
point([Player | Ts], Player, Res) :- point(Ts, Player, R), Res is R + 1.
point([_ | Ts], Player, Res) :- point(Ts, Player, Res).

% encapsulate
gridToLine(Grid, Res) :-
  gridToLine(Grid, [], Res).

% from a list of list get one list
gridToLine([], Res, Res).
gridToLine([Line|Grid], Line_tmp, Line_out) :-
  append(Line, Line_tmp, New_line),
  gridToLine(Grid, New_line, Line_out).

%% Count nb elem of Elm in Liste L
nb_elem([], X, X, _).
nb_elem([Elm|T], K, R, Elm) :-
  K1 is +(K,1),
  nb_elem(T,K1,R,Elm).

nb_elem([CmpNot|T], K, R, Elm) :-
  CmpNot \= Elm,
  nb_elem(T,K,R,Elm).

nb_elem(L, R, Elm):- nb_elem(L, 0 ,R, Elm).



%--------------- Heuristic ----------------------------%
heuristic(Grid, PlayerMax, PlayerMin, Res) :-
  playerPoint(Grid, PlayerMax, R1),
  playerPoint(Grid, PlayerMin, R2),
  coinParityHeuristic(Grid, PlayerMax, PlayerMin, R3),
  stabilityHeuristic(Grid, PlayerMax, PlayerMin, R4),
  Res is 25 * R3 + 25 * R4.

coinParityHeuristic(Grid, MaxPlayer, MinPlayer, Res) :-
  gridToLine(Grid, AsLine),
  nb_elem(AsLine, Nb_MaxCoins, MaxPlayer),
  nb_elem(AsLine, Nb_MinCoins, MinPlayer),
  Res is 100 * (Nb_MaxCoins - Nb_MinCoins) / (Nb_MaxCoins + Nb_MinCoins).

stability_weights([4,  -3,  2,  2,  2,  2, -3,  4,
                   -3, -4, -1, -1, -1, -1, -4, -3,
                   2,  -1,  1,  0,  0,  1, -1,  2,
                   2,  -1,  0,  1,  1,  0, -1,  2,
                   2,  -1,  0,  1,  1,  0, -1,  2,
                   2,  -1,  1,  0,  0,  1, -1,  2,
                   -3, -4, -1, -1, -1, -1, -4, -3,
                   4,  -3,  2,  2,  2,  2, -3,  4]).

stabilityHeuristic(Grid, MaxPlayer, MinPlayer, Res) :-
 gridToLine(Grid, AsLine),
 stability_weights(Stability_line),
 stabilityHeuristic_CB(AsLine, Stability_line,
                       MaxPlayer, MinPlayer,
                       Res_PlayerMax, Res_PlayerMin),

 Res is Res_PlayerMax - Res_PlayerMin.

stabilityHeuristic_CB([], [], _, _, 0, 0).

stabilityHeuristic_CB([Head_grid|Tail_grid], [Head_weights|Tail_weights],
                     /* MaxPlayer = */ Head_grid, MinPlayer,
                     Res_PlayerMax, Res_PlayerMin) :-

 stabilityHeuristic_CB(Tail_grid, Tail_weights,
                    Head_grid, MinPlayer,
                    Tmp_ResPlayerMax, Res_PlayerMin),!,

 Res_PlayerMax is Tmp_ResPlayerMax + Head_weights.

stabilityHeuristic_CB([Head_grid|Tail_grid], [Head_weights|Tail_weights],
                     MaxPlayer, /* MinPlayer = */ Head_grid,
                     Res_PlayerMax, Res_PlayerMin) :-

stabilityHeuristic_CB(Tail_grid, Tail_weights,
                  MaxPlayer, Head_grid,
                  Res_PlayerMax, Tmp_ResPlayerMin),!,

 Res_PlayerMin is Tmp_ResPlayerMin + Head_weights.

stabilityHeuristic_CB([_|TG], [_|TW], MaxPlayer, MinPlayer, ResMax, ResMin) :-
 stabilityHeuristic_CB(TG, TW, MaxPlayer, MinPlayer, ResMax, ResMin).
%--------------------------------------------------------------------------------------------------%

iter_elem([[Res | _] | _], 1, 1, Res).
iter_elem([[_ | ES] | TS], X, 1, Res) :- X2 is X - 1, iter_elem([ES | TS], X2, 1, Res).
iter_elem([_ | TS], X, Y, Res) :- Y2 is Y - 1, iter_elem(TS, X, Y2, Res).

setGrid([
["-", "-", "-", "-", "-", "-", "-", "-"],
["-", "-", "-", "-", "-", "-", "-", "-"],
["-", "-", "-", "-", "-", "-", "-", "-"],
["-", "-", "-", white, black, "-", "-", "-"],
["-", "-", "-", black, white, "-", "-", "-"],
["-", "-", "-", "-", "-", "-", "-", "-"],
["-", "-", "-", "-", "-", "-", "-", "-"],
["-", "-", "-", "-", "-", "-", "-", "-"]
]).

setGrid1([
["-", white, white, white, white, white, white, black],
[white, white, white, white, white, white, white, white],
[white, white, white, white, white, white, white, white],
[white, white, white, white, white, white, white, white],
[white, white, white, white, white, white, white, white],
[white, white, white, white, white, white, white, white],
[white, white, white, white, white, white, white, white],
[black, white, white, white, white, white, white, white]
]).

% --------------------- Changes -------------------------------------------%

change_Grid(X, Y, Player, Grid, Grid8) :-
  left_changes(X, Y, Player, Grid, Grid1),
  left_up_changes(X, Y, Player, Grid1, Grid2),
  up_changes(X, Y, Player, Grid2, Grid3),
  up_right_changes(X, Y, Player, Grid3, Grid4),
  right_changes(X, Y, Player, Grid4, Grid5),
  right_down_changes(X, Y, Player, Grid5, Grid6),
  down_changes(X, Y, Player, Grid6, Grid7),
  down_left_changes(X, Y, Player, Grid7, Grid8).

opposite_player(black, white).
opposite_player(white, black).

make_move(X, Y, Player, Grid, GridNew) :-
  permissible_move(X, Y, Player, Grid),
  move(X, Y, Player, Grid, GridNew).

move(X, Y, Player, Grid, GridNew) :-
  place(X, Y, Player, Grid, Grid_transition),
  change_Grid(X, Y, Player, Grid_transition, GridNew).

place2(1, Player, [_ | Es], [Player| Es]).
place2(X, Player, [E | Es], [E | Line]) :-
  X2 is X - 1, place2(X2, Player, Es, Line).

place(X, 1, Player, [E | Ts], [Line | Ts]) :-
  place2(X, Player, E, Line).
place(X, Y, Player, [E | Ts], [E | GridNew]) :-
  Y2 is Y - 1, place(X, Y2, Player, Ts, GridNew).

left_changes(X, Y, Player, Grid, GridNew) :- X2 is X - 1, Y2 is Y,
  not_edge(X2, Y2),
  not(empty_cell(X2, Y2, Grid)),
  iter_elem(Grid, X2, Y2, Res),
  opposite_player(Player, Opponent),
  Res = Opponent,
  left_change(Grid, X2, Y2, Player, Opponent, GridNew).
left_changes(_, _, _, Grid, Grid).
left_change(Grid, X, Y, _, Player, GridNew) :-
  not_edge(X, Y),
  not(empty_cell(X, Y, Grid)),
  iter_elem(Grid, X, Y, Res),
  opposite_player(Player, Opponent),
  Res = Opponent,
  GridNew = Grid.
left_change(Grid, X, Y, TrueColor, Player, GridNew) :-
  not_edge(X, Y),
  not(empty_cell(X, Y, Grid)),
  iter_elem(Grid, X, Y, Res),
  Res = Player,
  place(X,Y,TrueColor, Grid, Grid2),
  X2 is X - 1, Y2 is Y,
  left_change(Grid2, X2, Y2, TrueColor, Player, GridNew).


left_up_changes(X, Y, Player, Grid, GridNew) :- X2 is X - 1, Y2 is Y - 1,
  not_edge(X2, Y2),
  not(empty_cell(X2, Y2, Grid)),
  iter_elem(Grid, X2, Y2, Res),
  opposite_player(Player, Opponent),
  Res = Opponent,
  left_up_change(Grid, X2, Y2, Player, Opponent, GridNew).
left_up_changes(_, _, _, Grid, Grid).
left_up_change(Grid, X, Y, _, Player, GridNew) :-
  not_edge(X, Y),
  not(empty_cell(X, Y, Grid)),
  iter_elem(Grid, X, Y, Res),
  opposite_player(Player, Opponent),
  Res = Opponent,
  GridNew = Grid.
left_up_change(Grid, X, Y, TrueColor, Player, GridNew) :-
  not_edge(X, Y),
  not(empty_cell(X, Y, Grid)),
  iter_elem(Grid, X, Y, Res),
  Res = Player,
  place(X,Y,TrueColor, Grid, Grid2),
  X2 is X - 1, Y2 is Y - 1,
  left_up_change(Grid2, X2, Y2, TrueColor, Player, GridNew).


up_changes(X, Y, Player, Grid, GridNew) :- X2 is X, Y2 is Y - 1,
  not_edge(X2, Y2),
  not(empty_cell(X2, Y2, Grid)),
  iter_elem(Grid, X2, Y2, Res),
  opposite_player(Player, Opponent),
  Res = Opponent,
  up_change(Grid, X2, Y2, Player, Opponent, GridNew).
up_changes(_, _, _, Grid, Grid).
up_change(Grid, X, Y, _, Player, GridNew) :-
  not_edge(X, Y),
  not(empty_cell(X, Y, Grid)),
  iter_elem(Grid, X, Y, Res),
  opposite_player(Player, Opponent),
  Res = Opponent,
  GridNew = Grid.
up_change(Grid, X, Y, TrueColor, Player, GridNew) :-
  not_edge(X, Y),
  not(empty_cell(X, Y, Grid)),
  iter_elem(Grid, X, Y, Res),
  Res = Player,
  place(X,Y,TrueColor, Grid, Grid2),
  X2 is X, Y2 is Y - 1,
  up_change(Grid2, X2, Y2, TrueColor, Player, GridNew).


up_right_changes(X, Y, Player, Grid, GridNew) :- X2 is X + 1, Y2 is Y - 1,
  not_edge(X2, Y2),
  not(empty_cell(X2, Y2, Grid)),
  iter_elem(Grid, X2, Y2, Res),
  opposite_player(Player, Opponent),
  Res = Opponent,
  up_right_change(Grid, X2, Y2, Player, Opponent, GridNew).
up_right_changes(_, _, _, Grid, Grid).
up_right_change(Grid, X, Y, _, Player, GridNew) :-
  not_edge(X, Y),
  not(empty_cell(X, Y, Grid)),
  iter_elem(Grid, X, Y, Res),
  opposite_player(Player, Opponent),
  Res = Opponent,
  GridNew = Grid.
up_right_change(Grid, X, Y, TrueColor, Player, GridNew) :-
  not_edge(X, Y),
  not(empty_cell(X, Y, Grid)),
  iter_elem(Grid, X, Y, Res),
  Res = Player,
  place(X,Y,TrueColor, Grid, Grid2),
  X2 is X + 1, Y2 is Y - 1,
  up_right_change(Grid2, X2, Y2, TrueColor, Player, GridNew).


right_changes(X, Y, Player, Grid, GridNew) :- X2 is X + 1, Y2 is Y,
  not_edge(X2, Y2),
  not(empty_cell(X2, Y2, Grid)),
  iter_elem(Grid, X2, Y2, Res),
  opposite_player(Player, Opponent),
  Res = Opponent,
  right_change(Grid, X2, Y2, Player, Opponent, GridNew).
right_changes(_, _, _, Grid, Grid).
right_change(Grid, X, Y, _, Player, GridNew) :-
  not_edge(X, Y),
  not(empty_cell(X, Y, Grid)),
  iter_elem(Grid, X, Y, Res),
  opposite_player(Player, Opponent),
  Res = Opponent,
  GridNew = Grid.
right_change(Grid, X, Y, TrueColor, Player, GridNew) :-
  not_edge(X, Y),
  not(empty_cell(X, Y, Grid)),
  iter_elem(Grid, X, Y, Res),
  Res = Player,
  place(X,Y,TrueColor, Grid, Grid2),
  X2 is X + 1, Y2 is Y,
  right_change(Grid2, X2, Y2, TrueColor, Player, GridNew).


right_down_changes(X, Y, Player, Grid, GridNew) :- X2 is X + 1, Y2 is Y + 1,
  not_edge(X2, Y2),
  not(empty_cell(X2, Y2, Grid)),
  iter_elem(Grid, X2, Y2, Res),
  opposite_player(Player, Opponent),
  Res = Opponent,
  right_down_change(Grid, X2, Y2, Player, Opponent, GridNew).
right_down_changes(_, _, _, Grid, Grid).
right_down_change(Grid, X, Y, _, Player, GridNew) :-
  not_edge(X, Y),
  not(empty_cell(X, Y, Grid)),
  iter_elem(Grid, X, Y, Res),
  opposite_player(Player, Opponent),
  Res = Opponent,
  GridNew = Grid.
right_down_change(Grid, X, Y, TrueColor, Player, GridNew) :-
  not_edge(X, Y),
  not(empty_cell(X, Y, Grid)),
  iter_elem(Grid, X, Y, Res),
  Res = Player,
  place(X,Y,TrueColor, Grid, Grid2),
  X2 is X + 1, Y2 is Y + 1,
  right_down_change(Grid2, X2, Y2, TrueColor, Player, GridNew).

%

down_changes(X, Y, Player, Grid, GridNew) :- X2 is X, Y2 is Y + 1,
  not_edge(X2, Y2),
  not(empty_cell(X2, Y2, Grid)),
  iter_elem(Grid, X2, Y2, Res),
  opposite_player(Player, Opponent),
  Res = Opponent,
  down_change(Grid, X2, Y2, Player, Opponent, GridNew).
down_changes(_, _, _, Grid, Grid).
down_change(Grid, X, Y, _, Player, GridNew) :-
  not_edge(X, Y),
  not(empty_cell(X, Y, Grid)),
  iter_elem(Grid, X, Y, Res),
  opposite_player(Player, Opponent),
  Res = Opponent,
  GridNew = Grid.
down_change(Grid, X, Y, TrueColor, Player, GridNew) :-
  not_edge(X, Y),
  not(empty_cell(X, Y, Grid)),
  iter_elem(Grid, X, Y, Res),
  Res = Player,
  place(X,Y,TrueColor, Grid, Grid2),
  X2 is X, Y2 is Y + 1,
  down_change(Grid2, X2, Y2, TrueColor, Player, GridNew).


down_left_changes(X, Y, Player, Grid, GridNew) :- X2 is X - 1, Y2 is Y + 1,
  not_edge(X2, Y2),
  not(empty_cell(X2, Y2, Grid)),
  iter_elem(Grid, X2, Y2, Res),
  opposite_player(Player, Opponent),
  Res = Opponent,
  down_left_change(Grid, X2, Y2, Player, Opponent, GridNew).
down_left_changes(_, _, _, Grid, Grid).
down_left_change(Grid, X, Y, _, Player, GridNew) :-
  not_edge(X, Y),
  not(empty_cell(X, Y, Grid)),
  iter_elem(Grid, X, Y, Res),
  opposite_player(Player, Opponent),
  Res = Opponent,
  GridNew = Grid.
down_left_change(Grid, X, Y, TrueColor, Player, GridNew) :-
  not_edge(X, Y),
  not(empty_cell(X, Y, Grid)),
  iter_elem(Grid, X, Y, Res),
  Res = Player,
  place(X,Y,TrueColor, Grid, Grid2),
  X2 is X - 1, Y2 is Y + 1,
  down_left_change(Grid2, X2, Y2, TrueColor, Player, GridNew).
