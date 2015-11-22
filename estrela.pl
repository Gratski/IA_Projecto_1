%%-- A* BASE  %
%aStar_11((-9,1), [ [7, 0, (-9,1)] ] , (7,-9), [(-4,9),(-2,9),(0,-8),(8,1),(-8,1),(-6,5),(-8,-7),(0,5),(4,-9),(0,-5),(-5,-6),(2,-7),(-7,7),(0,-1),(-9,1),(-2,-3),(-8,6),(-1,1),(4,-4),(6,-5),(-3,-2),(0,-9),(-5,-9),(4,9),(4,-6),(2,-2),(7,9),(5,1),(2,6),(-5,-3),(8,-1),(2,-6),(-9,-1),(-3,9),(4,-1),(7,3),(-8,-1),(7,-9),(-7,-5),(-8,8),(1,-5),(4,-7),(-2,-5),(-4,-9),(5,-7),(-7,5),(6,-1),(2,-1),(5,-3),(-4,7),(-3,-5),(4,3),(0,-4),(-7,-6),(4,-5),(-6,-9),(-6,1),(-1,-1),(-4,1),(1,-3),(2,9),(0,7),(6,1),(-8,9),(4,1),(7,5),(-7,-1),(-3,5),(-3,1),(1,7),(-2,7),(1,3),(7,-3),(6,7),(-1,4),(-7,1),(0,-3),(-9,3),(-4,-5),(-5,-7),(2,-9),(4,8),(-5,6),(-2,3),(-8,5),(-7,3),(-8,-5),(1,-9),(4,5),(-5,8),(-1,-5),(-5,-4),(5,5),(-5,5),(0,9),(-1,-3),(7,-5),(-2,-9),(4,6),(-1,-9),(4,-3),(3,-9),(3,1),(-6,-7),(8,3),(5,-9),(-8,3),(5,7),(6,-7),(-5,-2),(7,7),(-1,-7),(-3,-9),(-8,-8),(-1,9),(7,-4),(2,0),(-5,-1),(-5,3),(7,6),(3,-3),(-3,-7),(6,-6),(-8,-9),(4,4),(7,1),(-4,-3),(2,-3),(-6,7),(-5,1),(-5,7),(-3,-3),(-3,-1),(1,9),(4,7),(-3,-6),(2,5),(2,7),(1,5),(7,-8),(2,-5),(7,-7),(0,4),(-7,-3),(0,3),(-6,-3),(-5,4),(-5,-5),(6,-3),(-3,0),(-1,-8),(4,-2),(-2,-1),(-1,7),(1,-1),(-8,-4),(-8,7),(0,-7),(-3,3),(-2,-7),(3,-5),(-3,6),(-7,-9),(-2,5),(2,3),(-1,3),(0,1),(2,1),(1,-7),(-5,2),(6,3),(-1,5),(3,9),(-7,9),(-3,2),(-6,9),(6,5),(2,2),(-3,7),(7,-1),(4,0),(-1,-4),(-8,-3),(-5,0),(-7,-7),(6,-9),(5,9),(3,7),(-1,8),(4,2),(0,8),(6,9),(-5,9),(7,8)], [], [], Sol).

aStar_11( Me, [ [ _, C, (X, Y) | R ] | _ ], Goal, _, _, _, Sol ):-
	manhatan_11( (X, Y), Goal, 0 ),
	reverse( [(X, Y) | R], [ EE, (PinX, PinY) | REV ]),
	write('Custo'), nl,
	write(C), nl,
	write('CAMINHO: '),nl,
	write([ EE, (PinX, PinY) | REV ]), nl,
	viz_11( Sol, Me, (PinX, PinY) ).

aStar_11( Me, [ [ H, C, (X, Y) | R ] | Tree ], Goal, Free, Visitados, Expandidos, Sol ):-

	%write('Reecebeu, Lista Ordenada'), nl,
	%write([ [H, C, (X, Y) | R] | HeadTT]), nl,

	sucs_11( (X, Y), Free, Visitados, SucsList ),

	%write('Lista de Sucessores'), nl,
	%write(SucsList), nl,

	SucsCusto is C + 1,
	applyHeuristic_11( SucsCusto , SucsList, Goal, SucsHeuristic ),

	%write('Sucs depois de heuristica'), nl,
	%write( SucsHeuristic ), nl,

	filter_11( SucsHeuristic, Expandidos, SucsFiltered),

	%write('Sucs depois de filtrados'), nl,
	%write( SucsFiltered ), nl,

	actualiza_expandidos( SucsFiltered, Expandidos, NewExpandidos ),

	%write('Expandidos Actualizados'), nl,
	%write( NewExpandidos ), nl,

	append_all( SucsFiltered, [ (X, Y) | R ], NewSubTree ),

	%write('Nova Sub Tree Gerada'), nl,
	%write( NewSubTree ), nl,

	append( NewSubTree, Tree, TTT ),

	setof( [ Hs, Cs, (Xs, Ys) | Rs ], Cs^Xs^Ys^member( [ Hs, Cs, (Xs, Ys) | Rs ], TTT ),  NNN ),

	%write('Nova Tree Concatenada'), nl,
	%write( NewSubTree ), nl,

	append( [(X, Y)], Visitados, NewVisitados ),

	write('Nova Tree'), nl,
	write( NNN ), nl,

	aStar_11( Me, NNN, Goal, Free, NewVisitados, Expandidos, Sol ).

aStar_11( _, _, _, _, _, _, 90 ).

%%-- ACTUALIZA EXPANDIDOS %
actualiza_expandidos( [], Exp, Exp).
actualiza_expandidos( [ [ H, _, (X, Y) ]|T ], Exp, Sol ):-
	\+member( (_, (X, Y)), Exp ),
	append( [(H, (X, Y))], Exp, NewExp ),
	actualiza_expandidos( T, NewExp, Sol ).

%%-- SUBSTITUI EXPANDIDOS %
actualiza_expandidos( [ [ H, _, (X, Y) ]|T ], Exp, Sol ):-
	member( (Hexp, (X, Y)), Exp ),
	substitui_expandidos( (H, (X, Y)), Exp, NewExp ),
	actualiza_expandidos( T, NewExp, Sol ).

substitui_expandidos( (H, (X, Y)), [ (He, (Xe, Ye)) | R ], Sol ):-
	(X \= Xe ; Y \= Ye),
	substitui_expandidos( (H, (X, Y)), R, Rec ),
	Sol = [ (He, (Xe, Ye)) | Rec ].
substitui_expandidos( (H, (X, Y)), [ (He, (Xe, Ye)) | R ], Sol ):-
	X == Xe, Y == Ye,
	Sol = [ (H, (X, Y)) | R ].

%%-- APPEND ALL %
append_all( [], _, [] ).
append_all( [ [H, C, (X,Y)] | R ], L, Sol ):-
	append( [H, C, (X,Y)], L, This ),
	append_all( R, L, Rec ),
	Sol = [ This | Rec].

%%-- FILTRA SUCS DE ACORDO COM EXPANDIDOS %
filter_11( [], _, [] ).
filter_11( [ [H, C, (X, Y)] | R], Expandidos, Sol ):-
	( \+member( (_,(X, Y)), Expandidos ) ; ( member( (Hc, (X, Y)), Expandidos ), Hc > H ) ),
	filter_11( R, Expandidos, Rec ),
	Sol = [ [H, C, (X, Y)] | Rec ].

filter_11( [ [H, C, (X, Y)] | R], Expandidos, Sol ):-
	member( (Hc, (X, Y)), Expandidos ), (Hc < H ; Hc == Hc),
	filter_11( R, Expandidos, Sol ).

%%-- APLICA HEURISTICA  %
applyHeuristic_11( _, [], _, [] ).
applyHeuristic_11( Custo, [ P| R ], Goal, Sol ):-
	manhatan_11( P , Goal, Dist ),
	Heuristica is Dist + Custo,
	applyHeuristic_11( Custo, R, Goal, Rec ),
	Sol = [ [Heuristica, Custo, P] | Rec ].

%%-- SUCESSORES %
sucs_11( Pos, Free, Visitados, R ):-
	findall( (SucX, SucY), ( viz_11(_, Pos, (SucX, SucY)), \+member( (SucX, SucY), Visitados ), member( (SucX, SucY), Free )), R ).

%%-- MANHANTAN %
manhatan_11( (X1, Y1), (X2, Y2), Dist ):-
	DistX is X1 - X2, abs(DistX, AbsX),
	DistY is Y1 - Y2, abs(DistY, AbsY),
	Dist is AbsX + AbsY.

%%-- VIZINHO %
viz_11(0,(X,Y),(X,NY)) :-
	NY is Y + 1.
viz_11(180,(X,Y),(X,NY)) :-
	NY is Y - 1.
viz_11(90,(X,Y),(NX,Y)) :-
	NX is X + 1.
viz_11(270,(X,Y),(NX,Y)) :-
	NX is X - 1.