%%%%%%%%%%%%%%%%%%
% OTHELLO PROLOG %
% BRANCHEREAU C. %
% GRAVEY THIBAUT %
%%%%%%%%%%%%%%%%%%

%Rules : https://www.ultraboardgames.com/othello/game-rules.php

%Objective : The goal is to get the majority of colour disks on the board at the end of the game.

%Creation of the boarding game at the begin of the play
%%The board will start with 2 black discs and 2 white discs at the centre of the board.
%%They are arranged with black forming a North-East to South-West direction.
%%White is forming a North-West to South-East direction.
%%Black always moves first.
:- writeln('Bienvenue sur Prolog_Othello-IA !').
:- dynamic(board/1).
:- retractall(board(_)).

init :- 
	length(Board,64),
	nth0(27,Board,'w'),
	nth0(28,Board,'b'),
	nth0(35,Board,'b'),
	nth0(36,Board,'w'),
	assertz(board(Board)),
	writeln('Initialisation du board OK'),
	displayBoard.

%Playing turn
%%if there is no winner, we made a normal turn for the next player
%%If you cant outflank and flip at least one opposing disc, you must pass 
%%your turn. However, if a move is available to you, you cant forfeit your turn.
%%if a player cannot make a valide move, he pass his turn and the opponent continues
play(_) :- gameover(Winner), !, writeln('Game is over.'), displayBoard.
play(Player) :- board(Board), canMakeAMove(Board,Player) , format('New turn for : ~w ~n',[Player]), displayBoard, 
				ia(Board,Player,Move), playMove(Board,Move,Player,NewBoard),applyIt(Board,NewBoard),switchPlayer(Player,NextPlayer), play(NextPlayer).
play(Player) :- format('Player "~w" can not play.~n',[Player]), changePlayer(Player,NextPlayer), play(NextPlayer).

%Check if a move is still available for the player
%%TODO : all (find one validMove then stop before backtrack)
canMakeAMove(Board,Player) :- 1==2.

%Get all valid moves for a player
allValidMoves(Board, Player, List) :- findAll(X, isValid(Board,Player,X), List).

%Check if a move is valid
isValid(Board,Player,Index) :- 
	emptyCell(Board,Index),
	(isSandwich(Board,Player,top);
	isSandwich(Board,Player,down);
	isSandwich(Board,Player,left);
	isSandwich(Board,Player,right);
	isSandwich(Board,Player,diagNW);
	isSandwich(Board,Player,diagNE);
	isSandwich(Board,Player,diagSE);
	isSandwich(Board,Player,diagSW)).


%Check if a cell is empty
emptyCell(Board,Index) :- nth0(Index,Board,X), var(X).

%Check in all direction if there is a sandwich (at least one opposite disk then a player disk)
%TODO :
isSandwich(Board,Player,direction) :- 1==2

%Play a regular move
playMove(Board, Move, Player, NewBoard) :- nth0(Move,Board,Player), flipper(Board,Move,Player,List), majBoard(Board,Player,List,NewBoard).

%Get the list of all flipped disk
flipper(Board,Move,Player,List) :- 
	flip(Board,Move,Player,top,L1),
	flip(Board,Move,Player,down,L2),
	flip(Board,Move,Player,left,L3),
	flip(Board,Move,Player,right,L4),
	flip(Board,Move,Player,diagNW,L5),
	flip(Board,Move,Player,diagNE,L6),
	flip(Board,Move,Player,diagSE,L7),
	flip(Board,Move,Player,diagSW,L8),
	append([L1,L2,L3,L4,L5,L6,L7,L8],List).


%Try to Flip in a precise direction, give the flipped disk index (list)
%TODO
flip(Board,Move,Player,direction,List) :- 1==2.

%Maj the board with by flipping the disk in the list
majBoard(Board,Player,[],NewBoard) :- NewBoard = Board.
majBoard(Board,Player,[H|T],NewBoard) :- nth0(H,Board,Player), majBoard(Board,Player,[T],NewBoard).

%Implement IA
%%TODO : Different algorithm, here is a random move from the list
ia(Board,Player, Move) :- allValidMoves(Board,Player,List), length(List,Length), random(0,Length, Index), nth0(Index,List,Move).

%Save the new board and remove the old one from the knowledge base
applyIt(Board,NewBoard) :- 
	retract(board(Board)),
	assertz(board(NewBoard)).

%Switch player
switchPlayer('b','w').
switchPlayer('w','b').

%End of the game
%%When it is no longer possible for either player to move, the game is over.
%%The discs are now counted and the player with the majority of his or her color 
%%discs on the board is the winner.
%%A tie is possible.
gameover(Winner) :- board(Board), \+ moveAvailable(Board,P), findWinner(Board,Winner).

%Find the winner
findWinner(Board, Winner):- countDisk(Board,0,0,B,W), selectWinner(B,W,Winner),
	format('~w black disks against ~w white disks.~nThe winner is player ~w !~n',[B,W,Winner]).
	
%Count the number of disk for each player B and W
countDisk([],B,W,FinalB,FinalW) :- FinalB is B, FinalW is W.
countDisk([H|T],B,W,FinalB,FinalW) :- H == 'b', X is B+1, countDisk(T,X,W,FinalB,FinalW).
countDisk([H|T],B,W,FinalB,FinalW) :- H == 'w', Y is W+1, countDisk(T,B,Y,FinalB,FinalW).
countDisk([_|T],B,W,FinalB,FinalW) :- countDisk(T,B,W,FinalB,FinalW).

%Select the winner depends on B and W the count of the disk
selectWinner(B,W,Winner) :- B=:=W, Winner='Draw'.
selectWinner(B,W,Winner) :- B=\=W, B<W, Winner='White'.
selectWinner(B,W,Winner) :- B=\=W, B>W, Winner='Black'.

%Display the othello board
displayBoard :- writeln('--------'), board(Board), displayRow(Board,0), writeln('--------').

displayRow([],_) :- writeln('').
displayRow(Board,8) :- writeln(''), displayRow(Board,0).
displayRow([H|T],X) :- Y is X+1, display(H), displayRow(T,Y).

display(Elem) :- var(Elem), write('O').
display(Elem) :- write(Elem).