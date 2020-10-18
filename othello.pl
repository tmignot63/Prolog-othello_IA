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
play(Player) :- board(Board), moveAvailable(Board,Player) , write('New turn for : '), writeln(Player), displayBoard, 
				ia(Board,Move,Player), playMove(Board,Move,NewBoard,Player),applyIt(Board,NewBoard),switchPlayer(Player,NextPlayer), play(NextPlayer).
play(Player) :- write('Player "'), write(Player), writeln('" can not play.'), changePlayer(Player,NextPlayer), play(NextPlayer).

%Check if a move is still available for the player
%%TODO : En totalité
moveAvailable(Board,Player) :- 1==2.

%Implement IA
%%TODO : Different algorithm
ia(Board,Move,Player) :- 1==2.

%Play a regular move
%%TODO : ADD THE DISK AND RETURN THE OTHER
playMove(Board, Move, NewBoard, Player) :- 1==2.

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