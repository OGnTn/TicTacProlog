:- use_module([library(lists),io,fill]).
is_cross(x).
is_nought(o).
is_empty(#).
is_piece(X) :-
    is_cross(X).
is_piece(O) :-
    is_nought(O).
other_player(X,O) :- 
    is_cross(X),
    is_nought(O).
other_player(O, X) :-
    is_nought(O),
    is_cross(X).

empty_board([ e(1,1,#),e(2,1,#),e(3,1,#),
                e(1,2,#),e(2,2,#),e(3,2,#),
                e(1,3,#),e(2,3,#),e(3,3,#) ]).

initial_board(Board) :-
    empty_board(Board).

in_bounds(1).
in_bounds(2).
in_bounds(3).

row(Y,Board,row(Y,A,B,C)) :-
    in_bounds(Y),
    is_list(Board),
    member(e(1,Y,A), Board),
    member(e(2,Y,B), Board),
    member(e(3,Y,C), Board).

column(X,Board,column(X,A,B,C)) :-
    in_bounds(X),
    is_list(Board),
    member(e(X,1,A), Board),
    member(e(X,2,B), Board),
    member(e(X,3,C), Board).

diagonal(top_to_bottom,Board,dia(top_to_bottom,A,B,C)) :-
    is_list(Board),
    member(e(1,1,A), Board),
    member(e(2,2,B), Board),
    member(e(3,3,C), Board).

diagonal(bottom_to_top,Board,dia(bottom_to_top,A,B,C)) :-
    is_list(Board),
    member(e(3,1,A), Board),
    member(e(2,2,B), Board),
    member(e(1,3,C), Board).

square(X, Y, Board, squ(X, Y, Piece)) :-
    in_bounds(X),
    in_bounds(Y),
    member(e(X, Y, Piece), Board).

empty_square(X, Y, Board) :-
    is_empty(Piece),
    square(X, Y, Board, squ(X, Y, Piece)).
corner_square(X, Y, Board) :-
    X = 1; X = 3, Y = 1; Y = 3,
    square(X, Y, Board, squ(X, Y, Piece)).
    

and_the_winner_is(Board, W) :-
    is_piece(W),
    winner(Board, W).

winner(Board, W) :-
    row(_, Board, row(_, W, W, W)).
winner(Board, W) :-
    column(_, Board, column(_, W, W, W)).
winner(Board, W) :-

    diagonal(_, Board, dia(_, W, W, W)).

rowwin([e(1,1,#),e(2,1,#),e(3,1,#),
        e(1,2,x),e(2,2,x),e(3,2,x),
        e(1,3,#),e(2,3,#),e(3,3,#) ]).

columnwin([ e(1,1,#),e(2,1,#),e(3,1,o),
            e(1,2,#),e(2,2,#),e(3,2,o),
            e(1,3,#),e(2,3,#),e(3,3,o) ]).

diawin([e(1,1,o),e(2,1,#),e(3,1,o),
        e(1,2,#),e(2,2,o),e(3,2,o),
        e(1,3,#),e(2,3,#),e(3,3,o) ]).

no_more_free_squares(Board) :-
    is_empty(Piece),
    \+ member(e(_,_,Piece), Board).

put_piece(Piece, X, Y, Board, NewBoard) :-
    select(e(X, Y, #), Board, Deleted),
    append(Deleted, [e(X, Y, Piece)], NewBoard).

playHH(Piece, Board) :-
    and_the_winner_is(Board, W),
    display_board(Board),
    report_winner(W).
playHH(Piece, Board) :-

    no_more_free_squares(Board),
    display_board(Board),
    report_stalemate().

playHH(Piece, Board) :-
    \+ no_more_free_squares(Board),
    is_cross(Piece),
    get_legal_move(Piece, X, Y, Board),
    put_piece(Piece, X, Y, Board, NewBoard),
    display_board(NewBoard),
    is_nought(Z),
    playHH(Z, NewBoard).

playHH(Piece, Board) :-
    \+ no_more_free_squares(Board),
    is_nought(Piece),
    get_legal_move(Piece, X, Y, Board),
    put_piece(Piece, X, Y, Board, NewBoard),
    display_board(NewBoard),
    is_cross(Z),
    playHH(Z, NewBoard).

playHH :- 
    welcome(),
    initial_board(Board),
    display_board(Board),
    is_cross(Cross),
    playHH(Cross, Board).

%Choose the winning move.
choose_move(Piece, X, Y, Board) :-
    put_piece(W, X, Y, Board, NewBoard),
    and_the_winner_is(NewBoard, W),
    \+ other_player(Piece, W),
    empty_square( X, Y, Board ).

%Block the opposing winning move.
choose_move(Piece, X, Y, Board) :-
    put_piece(W, X, Y, Board, NewBoard),
    and_the_winner_is(NewBoard, W),
    other_player(Piece, W),
    empty_square( X, Y, Board ).

%Choose the middle square.
choose_move(Piece, X, Y, Board) :-
    X = 2, Y = 2,
    empty_square(X, Y, Board).

%Choose a corner square.
choose_move(Piece, X, Y, Board) :-
    corner_square(X, Y, Board),
    empty_square(X, Y, Board).

%choose arbitrarily.
choose_move(Piece, X, Y, Board ) :- 
    empty_square( X, Y, Board ).

playHC(Piece, Board) :-
    and_the_winner_is(Board, W),
    display_board(Board),
    report_winner(W).

playHC(Piece, Board) :-
    no_more_free_squares(Board),
    display_board(Board),
    report_stalemate().

playHC(Piece, Board):-
    \+ no_more_free_squares(Board),
    is_cross(Piece),
    get_legal_move(Piece, X, Y, Board),
    put_piece(Piece, X, Y, Board, NewBoard),
    display_board(NewBoard),
    is_nought(Z),
    playHC(Z, NewBoard).

playHC(Piece, Board) :-
    \+ no_more_free_squares(Board),
    is_nought(Piece),
    choose_move(Piece, X, Y, Board),
    put_piece(Piece, X, Y, Board, NewBoard),
    display_board(NewBoard),
    is_cross(Z),
    playHC(Z, NewBoard).

playHC :-
    welcome(),
    initial_board(Board),
    display_board(Board),
    is_cross(Cross),
    playHC(Cross, Board).

possible_win(Piece, Board) :-
    and_the_winner_is(Board, W).

possible_win(Piece, Board) :-
    put_piece(Piece, X, Y, Board, NewBoard),
    other_player(Piece, O),
    possible_win(O, NewBoard).


playSS(Piece, Board) :-
    and_the_winner_is(Board, W),
    display_board(Board),
    report_winner(W).

playSS(Piece, Board) :-
    no_more_free_squares(Board),
    display_board(Board),
    report_stalemate().

playSS(Piece, Board) :-
    \+ possible_win(Piece, Board),
    display_board(Board),
    report_stalemate().

playSS(Piece, Board):-
    \+ no_more_free_squares(Board),
    is_cross(Piece),
    get_legal_move(Piece, X, Y, Board),
    put_piece(Piece, X, Y, Board, NewBoard),
    display_board(NewBoard),
    is_nought(Z),
    playSS(Z, NewBoard).

playSS(Piece, Board) :-
    \+ no_more_free_squares(Board),
    is_nought(Piece),
    choose_move(Piece, X, Y, Board),
    put_piece(Piece, X, Y, Board, NewBoard),
    display_board(NewBoard),
    is_cross(Z),
    playSS(Z, NewBoard).

playSS :-
    welcome(),
    initial_board(Board),
    display_board(Board),
    is_cross(Cross),
    playSS(Cross, Board).