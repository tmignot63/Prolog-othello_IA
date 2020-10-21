% LIA selon la méthode minimax
% Elle cherche les différents coups à jouer sur un coup davance donné. Puis sélectionne le chemin qui mène vers le meilleur selon lheuristique choisi.

minimax(Pos, Move, Depth,Player) :- minimax(Depth, Pos,Player, _, Move).

minimax(0, Position, Player, Value, _) :- 
      heuristic(Position,V),
      Value is V*Player. 

minimax(D, Position,1, Value, Move) :-
      D > 0, 
      D1 is D - 1,
      allValidMoves(Position,b,Moves),
      minimax(Moves, Position, D1, 1, -1000, nil, Value, Move).

minimax(D, Position,-1, Value, Move) :-
      D > 0, 
      D1 is D - 1,
      allValidMoves(Position,w,Moves),
      minimax(Moves, Position, D1, -1, -1000, nil, Value, Move).

minimax([], _, _, _, Value, Best, Value, Best).

minimax([Move|Moves],Position,D,1, Value0,Move0,BestValue,BestMove):-

	getCopie(Position,PositionCopie),

	playMove(Position,Move,b,Position1),

    Opponent is -1,

    minimax(D, Position1, Opponent, OppValue, _OppMove), 

    Value is -OppValue,
    ( Value > Value0 ->        
      minimax(Moves,PositionCopie,D,1, Value ,Move ,BestValue,BestMove) ; 
      minimax(Moves,PositionCopie,D,1, Value0,Move0,BestValue,BestMove)
    ). 

 minimax([Move|Moves],Position,D,-1, Value0,Move0,BestValue,BestMove):-

 	getCopie(Position,PositionCopie),

	playMove(Position,Move,w,Position1),

    Opponent is 1,

    minimax(D, Position1, Opponent, OppValue, _OppMove), 

    Value is -OppValue,
    ( Value > Value0 ->        
      minimax(Moves,PositionCopie,D,-1, Value ,Move ,BestValue,BestMove) ; 
      minimax(Moves,PositionCopie,D,-1, Value0,Move0,BestValue,BestMove)
    ). 