:- use_module([library(lists),io,fill]).

%-------------------------------------------------
%---------------Declarative Programming-----------
%---------------Formative Assignment 1------------
%--------------------Tic Tac Toe------------------
%----------------Author: Nathan Pesch-------------
%----------email: nathan.david.pesch@vub.be-------
%-------------------------------------------------

%Setup Facts
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

%The board is represented by entries with their coordinates. This list need not be sorted.
empty_board([   e(1,1,#),e(2,1,#),e(3,1,#),
                e(1,2,#),e(2,2,#),e(3,2,#),
                e(1,3,#),e(2,3,#),e(3,3,#) ]).

initial_board(Board) :-
    empty_board(Board).

%Stating what is in bounds, everything else will be out of bounds.
in_bounds(1).
in_bounds(2).
in_bounds(3).

%Stating what are corner squares.
corner(1).
corner(3).

%Stating what are middle squares.
middle(2).

%Uses member to search the board list to find the Yth row.
row(Y,Board,row(Y,A,B,C)) :-
    in_bounds(Y),
    is_list(Board),
    member(e(1,Y,A), Board),
    member(e(2,Y,B), Board),
    member(e(3,Y,C), Board).

%Uses member to search the board list to find the Xth column.
column(X,Board,column(X,A,B,C)) :-
    in_bounds(X),
    is_list(Board),
    member(e(X,1,A), Board),
    member(e(X,2,B), Board),
    member(e(X,3,C), Board).

%Uses member to search the board list to find the top_to_bottom diagonal.
diagonal(top_to_bottom,Board,dia(top_to_bottom,A,B,C)) :-
    is_list(Board),
    member(e(1,1,A), Board),
    member(e(2,2,B), Board),
    member(e(3,3,C), Board).

%Uses member to search the board list to find the bottom_to_top diagonal.
diagonal(bottom_to_top,Board,dia(bottom_to_top,A,B,C)) :-
    is_list(Board),
    member(e(3,1,A), Board),
    member(e(2,2,B), Board),
    member(e(1,3,C), Board).

%Finds what piece is at coord X and Y, when they are within bounds.
square(X, Y, Board, squ(X, Y, Piece)) :-
    in_bounds(X),
    in_bounds(Y),
    member(e(X, Y, Piece), Board).

%Finds an empty square at coords X and Y.
empty_square(X, Y, Board) :-
    in_bounds(X),
    in_bounds(Y),
    is_empty(Piece),
    square(X, Y, Board, squ(X, Y, Piece)).

%squares at the corners of the board, used in one of the choose_move heuristics.
corner_square(X, Y, Board) :-
    corner(X),
    corner(Y),
    square(X, Y, Board, squ(X, Y, Piece)).
    
%Checks if any piece, not the blank piece, is in a winning formation.
and_the_winner_is(Board, W) :-
    is_piece(W),
    winner(Board, W).

%Is there a row where each piece is the same.
winner(Board, W) :-
    row(_, Board, row(_, W, W, W)).

%Is there a column where each piece is the same.
winner(Board, W) :-
    column(_, Board, column(_, W, W, W)).

%Is there a diagonal where each piece is the same.
winner(Board, W) :-
    diagonal(_, Board, dia(_, W, W, W)).

%Is true whenever no square of the board is the empty piece.
no_more_free_squares(Board) :-
    is_empty(Piece),
    \+ member(e(_,_,Piece), Board).

%Uses select to find the blank piece at coordinates X and Y and delete it from the original board,
%then appending an entry with the piece to this new board. 
%This results in the original board with one of the empty squares filled in with the specified piece.
put_piece(Piece, X, Y, Board, NewBoard) :-
    select(e(X, Y, #), Board, Deleted),
    append(Deleted, [e(X, Y, Piece)], NewBoard).

%-------------------------------------------------
%--------------------PlayHH-----------------------
%---------Play against another player-------------
%-------------------------------------------------

%First clause of playHH checks whether there is a winner, and reports this one.
playHH(Piece, Board) :-
    and_the_winner_is(Board, W),
    display_board(Board),
    report_winner(W).

%Second clause checks whether there are no more free squares on the board. If this results to true, a stalemate is reported.
playHH(Piece, Board) :-
    no_more_free_squares(Board),
    display_board(Board),
    report_stalemate().

%Third clause gets a legal move from the current player, puts the piece, and switches the player.
playHH(Piece, Board) :-
    get_legal_move(Piece, X, Y, Board),
    put_piece(Piece, X, Y, Board, NewBoard),
    display_board(NewBoard),
    other_player(Piece, Z),
    playHH(Z, NewBoard).

%Start clause for playHH. Initialize the board and start the game with player cross.
playHH :- 
    welcome(),
    initial_board(Board),
    display_board(Board),
    is_cross(Cross),
    playHH(Cross, Board).

%-------------------------------------------------
%--------------------PlayHC-----------------------
%----------Play against the computer--------------
%------------No stalemate detection---------------
%-------------------------------------------------

%The following two clauses could be 1 clause, but it is split in two to give priority to 
%taking the the winning move if both scenarios would be active at one time.
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
    middle(X),
    middle(Y),
    empty_square(X, Y, Board).

%Choose a corner square.
choose_move(Piece, X, Y, Board) :-
    corner_square(X, Y, Board),
    empty_square(X, Y, Board).

%choose arbitrarily.
choose_move(Piece, X, Y, Board ) :- 
    empty_square( X, Y, Board ).

%True if there is a winning formation on the board, reports it.
playHC(Piece, Board) :-
    and_the_winner_is(Board, W),
    display_board(Board),
    report_winner(W).

%True if all squares are occupied, reports stalemate.
playHC(Piece, Board) :-
    no_more_free_squares(Board),
    display_board(Board),
    report_stalemate().

%Gets a legal move from the user, and switches to the computer.
playHC(Piece, Board):-
    is_cross(Piece),
    get_legal_move(Piece, X, Y, Board),
    put_piece(Piece, X, Y, Board, NewBoard),
    display_board(NewBoard),
    is_nought(Z),
    playHC(Z, NewBoard).

%Computer chooses a move and switches to the player.
playHC(Piece, Board) :-
    is_nought(Piece),
    choose_move(Piece, X, Y, Board),
    put_piece(Piece, X, Y, Board, NewBoard),
    display_board(NewBoard),
    is_cross(Z),
    playHC(Z, NewBoard).

%starting clause for playHC.
playHC :-
    welcome(),
    initial_board(Board),
    display_board(Board),
    is_cross(Cross),
    playHC(Cross, Board).

%-------------------------------------------------
%--------------------PlaySS-----------------------
%----------Play against the computer--------------
%-------------Stalemate detection-----------------
%-------------------------------------------------

%First possible win clause, is true whenever there is a winning formation on the given Board.
possible_win(Piece, Board) :-
    and_the_winner_is(Board, W).

%Second win clause, recursively puts pieces on the board, thus simulating all possible futures. 
%Is true if anywhere in the future a winning formation is found due to clause above
%Is false after exhausting every possible future configuration and no win is found.

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

%If possible win is false, this clause will be true and this a stalemate will be reported.
playSS(Piece, Board) :-
    \+ possible_win(Piece, Board),
    display_board(Board),
    report_stalemate().

playSS(Piece, Board):-
    is_cross(Piece),
    get_legal_move(Piece, X, Y, Board),
    put_piece(Piece, X, Y, Board, NewBoard),
    display_board(NewBoard),
    is_nought(Z),
    playSS(Z, NewBoard).

playSS(Piece, Board) :-
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

%-------------------------------------------------
%----------------------Tests----------------------
%-------------------------------------------------

test_first_move :-
    is_nought(O),
    choose_move(O, X, Y, [  e(1,1,#),e(2,1,#),e(3,1,#),
                            e(1,2,#),e(2,2,#),e(3,2,#),
                            e(1,3,#),e(2,3,#),e(3,3,#) ]),
    X = 2, Y = 2. %The move to be picked should be (2,2).

test_second_move :-
    is_nought(O),
    choose_move(O, X, Y, [  e(1,1,#),e(2,1,#),e(3,1,#),
                            e(1,2,#),e(2,2,x),e(3,2,#),
                            e(1,3,#),e(2,3,#),e(3,3,#) ]),
    corner(X), corner(Y). %The move to be picked should be in a corner.

test_winning_move :-
    is_nought(O),
    choose_move(O, X, Y, [  e(1,1,o),e(2,1,o),e(3,1,#),
                            e(1,2,#),e(2,2,x),e(3,2,#),
                            e(1,3,#),e(2,3,#),e(3,3,#) ]),
    X = 3, Y = 1. %The move to be picked should be top right.

test_blocking_move :-
    is_nought(O),
    choose_move(O, X, Y, [  e(1,1,#),e(2,1,#),e(3,1,#),
                            e(1,2,#),e(2,2,x),e(3,2,x),
                            e(1,3,#),e(2,3,#),e(3,3,#) ]),
    X = 1, Y = 2. %The move to be picked should be middle left.

test_winner_row :-
    and_the_winner_is([ e(1,1,#),e(2,1,#),e(3,1,#),
                        e(1,2,x),e(2,2,x),e(3,2,x),
                        e(1,3,#),e(2,3,#),e(3,3,#) ],
                        W),
    is_cross(W). %cross should be detected as the winner.

test_winner_column :-
    and_the_winner_is([ e(1,1,o),e(2,1,#),e(3,1,#),
                        e(1,2,o),e(2,2,x),e(3,2,x),
                        e(1,3,o),e(2,3,#),e(3,3,#) ],
                        W),
    is_nought(W). %Nought should be detected as the winner.

test_winner_dia :-
    and_the_winner_is([ e(1,1,o),e(2,1,#),e(3,1,x),
                        e(1,2,o),e(2,2,x),e(3,2,x),
                        e(1,3,x),e(2,3,#),e(3,3,#) ],
                        W),
    is_cross(W). %Cross should be detected as the winner.

test_empty_square :-
    empty_square(X, Y, [    e(1,1,o),e(2,1,o),e(3,1,x),
                            e(1,2,o),e(2,2,x),e(3,2,x),
                            e(1,3,x),e(2,3,#),e(3,3,x) ]),
    X = 2, Y = 3. %The empty square to be found should be (2, 3).

test_possible_win :-
    possible_win(Piece, [   e(1,1,#),e(2,1,x),e(3,1,#),
                            e(1,2,o),e(2,2,x),e(3,2,x),
                            e(1,3,x),e(2,3,#),e(3,3,x) ]),
    is_cross(Piece). %A possible win should be detected for cross.

test_impossible_win :-
    \+ possible_win(Piece, [   e(1,1,x),e(2,1,x),e(3,1,o),
                            e(1,2,o),e(2,2,#),e(3,2,x),
                            e(1,3,x),e(2,3,#),e(3,3,o) ]). 
                %No possible win should be detected.

run_all_tests :-
    test_first_move,
    test_second_move,
    test_winning_move,
    test_blocking_move,
    test_winner_row,
    test_winner_column,
    test_winner_dia,
    test_possible_win,
    test_impossible_win,
    test_empty_square.