%pacman(Clock, ClockLimit, Score, Me, Partner, OtherTeam, HomeBase , HisBase, FreeCells, MyP, MYSupP, HisP, HisSupP, Decisao)
%pacman(Clock, ClockLimit, Score, Me, Partner, OtherTeam, HomeBase , HisBase, FreeCells, MyP, MYSupP, HisP, HisSupP, Decisao):-
	

%%----------------------------------------------- Heuristic methods %
applyHeuristic_11( Me, Goals, Enemies, Result):-
	applyHeuristic_aux_11( Me, Goals, Goals, Enemies, Result).

applyHeuristic_aux_11( _, [], _, _, []).
applyHeuristic_aux_11( Me, [P|R], Goals, [Enemy1, Enemy2], Result):-
	eval_distance( Me, P, Distance_Factor ),
	eval_vizinhanca_11( P, Goals, Nei_Factor ),
	HeuristicValue is (( (Distance_Factor * 0.6 ) - (Nei_Factor * 0.4) ) / 2),
	applyHeuristic_aux_11( Me, R, Goals, [Enemy1, Enemy2], Result_Rec ),
	Result = [ ( HeuristicValue, P) | Result_Rec ].


%%-------------- Avalia factor de vizinhanca %
eval_vizinhanca_11( P, L, R ):-
	findall( Viz, ( viz(_, P, Viz), member( Viz, L ) ), List ),
	length(List, R).

%%-------------- Avalia factor de perigo %
eval_danger( Pos1, Pos2, Res ):-
	manhatan( Pos1, Pos2, Calc ),
	parse_danger_value(Calc, Res).

%interpreta o valor de perigo
parse_danger_value(Val, -1):-
	Val >= 2.
parse_danger_value(Val, 20):-
	Val < 2.

%%-------------- Avalia factor de distancia %
eval_distance( Pos1, Pos2, Res):-
	manhatan(Pos1, Pos2, Res).



%%----------------------------------------------- Aux Methods %
%concatena duas listas	
concat_11(L1, L2, Res):-
	append(L1, L2, Res).

%calcula distancia de manhatan
manhatan( (X1, Y1), (X2, Y2), Dist ):-
	DistX is X1 - X2, abs(DistX, AbsX),
	DistY is Y1 - Y2, abs(DistY, AbsY),
	Dist is AbsX + AbsY.


%vizinhos
viz(0,(X,Y),(X,NY)) :-
	NY is Y + 1.
viz(180,(X,Y),(X,NY)) :-
	NY is Y - 1.
viz(90,(X,Y),(NX,Y)) :-
	NX is X + 1.
viz(270,(X,Y),(NX,Y)) :-
	NX is X - 1.