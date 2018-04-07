% --------------------- Display Board Routines ----------------------------------------------------%
display_board(Picture, Grid) :-
  new(Box, box(320, 320)),
  send(Box, fill_pattern, colour(yellow)),
  send(Picture, display, Box, point(0,0)),
  display_edge(Picture),
  % write(Grid),
  % write(List),
  % board_psb_move(Grid, Player, List, GridNew),
  display_brd(Picture, 0, 0, Grid).

board_psb_move(Grid, black, [], Grid).
board_psb_move(Grid, black, [[X | Y] | T], GridNew2) :-
  move(X, Y, "b", Grid, GridNew),
  board_psb_move(GridNew, black, T, GridNew2).
board_psb_move(Grid, white, [], Grid).
board_psb_move(Grid, white, [[X | Y] | T], GridNew2) :-
  move(X, Y, "w", Grid, GridNew),
  board_psb_move(GridNew, white, T, GridNew2).

display_edge(Picture) :-
  send(Picture, display, new(_, text("1")), point(15,320)),
	send(Picture, display, new(_, text("2")), point(55,320)),
	send(Picture, display, new(_, text("3")), point(95,320)),
	send(Picture, display, new(_, text("4")), point(135,320)),
	send(Picture, display, new(_, text("5")), point(175,320)),
	send(Picture, display, new(_, text("6")), point(215,320)),
	send(Picture, display, new(_, text("7")), point(255,320)),
	send(Picture, display, new(_, text("8")), point(295,320)),
	send(Picture, display, new(_, text("1")), point(320,15)),
	send(Picture, display, new(_, text("2")), point(320,55)),
	send(Picture, display, new(_, text("3")), point(320,95)),
	send(Picture, display, new(_, text("4")), point(320,135)),
	send(Picture, display, new(_, text("5")), point(320,175)),
	send(Picture, display, new(_, text("6")), point(320,215)),
	send(Picture, display, new(_, text("7")), point(320,255)),
	send(Picture, display, new(_, text("8")), point(320,295)).

display_brd(_, _, _, []).
display_brd(Picture, X, Y, [Es | Ts]) :-
  display_line(Picture, X, Y, Es),
  Y2 is Y + 40, display_brd(Picture, X, Y2, Ts).

display_line(_, _, _, []).
display_line(Picture, X, Y, [E | Es]) :-
  display_square(Picture, X, Y, E),
  X2 is X + 40, display_line(Picture, X2, Y, Es).

display_square(Picture, X, Y, "-") :-
  draw_box(Picture, X, Y).

display_square(Picture, X, Y, "b") :-
  send(Picture, display, new(Circle, circle(10)), point(X + 20, Y + 20)),
  send(Circle, fill_pattern, colour(black)),
  draw_box(Picture, X, Y).

display_square(Picture, X, Y, "w") :-
  send(Picture, display, new(Circle, circle(10)), point(X + 20, Y + 20)),
  send(Circle, fill_pattern, colour(white)),
  draw_box(Picture, X, Y).

display_square(Picture, X, Y, black) :-
  draw_circle(Picture, X, Y, black),
  draw_box(Picture, X, Y).

display_square(Picture, X, Y, white) :-
  draw_circle(Picture, X, Y, white),
  draw_box(Picture, X, Y).

draw_circle(Picture, X, Y, Color) :-
  send(Picture, display, new(Circle, ellipse(40,40)), point(X,Y)),
  send(Circle, fill_pattern, colour(Color)).

draw_box(Picture, X, Y) :-
  send(Picture, display, new(Rectan, box(40,40)), point(X,Y)),
  send(Rectan, colour, colour(black)).

%--------------------------------------------------------------------------------------------------%
