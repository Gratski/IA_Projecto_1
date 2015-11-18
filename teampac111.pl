%pacman(Clock, ClockLimit, Score, Me, Partner, OtherTeam, HomeBase , HisBase, FreeCells, MyP, MYSupP, HisP, HisSupP, Decisao)
%pacman(Clock, ClockLimit, Score, Me, Partner, OtherTeam, HomeBase , HisBase, FreeCells, MyP, MYSupP, HisP, HisSupP, Decisao):-
	
	

%%----------------------------------------------- Heuristic methods %
applyHeuristic_11( Me, Goals, Enemies, Result):-
	applyHeuristic_aux_11( Me, Goals, Goals, Enemies, Result).

applyHeuristic_aux_11( _, [], _, _, []).
applyHeuristic_aux_11( Me, [P|R], Goals, [Enemy1, Enemy2], Result):-
	eval_danger( P, Enemy1, Danger_Factor_1 ), eval_danger( P, Enemy2, Danger_Factor_2 ),
	eval_distance( Me, P, Distance_Factor ),
	HeuristicValue is ( Distance_Factor + Danger_Factor_1 + Danger_Factor_2 ),
	applyHeuristic_aux_11( Me, R, Goals, [Enemy1, Enemy2], Result_Rec ),
	Result = [ ( HeuristicValue, P) | Result_Rec ].


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

%%-------------- Avalia factor de vizinhança %
%falta fazer



%%----------------------------------------------- Aux Methods %
%concatena duas listas	
concat_11(L1, L2, Res):-
	append(L1, L2, Res).

%calcula distancia de manhatan
manhatan( (X1, Y1), (X2, Y2), Dist ):-
	DistX is X1 - X2, abs(DistX, AbsX),
	DistY is Y1 - Y2, abs(DistY, AbsY),
	Dist is AbsX + AbsY.
