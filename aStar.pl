%-- [ [ H, C, Head | SubTree ] | UpTree ]

%pacman11(1,300,0,(1,-9,1,90,0),(0,-9,1,90,0),[(2,8,1,270,0),(3,7,1,270,0)],(-9,1),(8,1),[(-7,7),(4,-1),(-5,-3),(3,-9),(-3,-3),(-1,-9),(-4,-5),(0,9),(-3,-7),(-3,7),(4,4),(7,7),(-3,-2),(-7,-1),(0,-5),(-2,9),(-7,-9),(4,-4),(-5,-2),(0,8),(-4,1),(-5,6),(5,5),(4,-9),(-4,7),(-2,-7),(-8,-1),(4,1),(1,-9),(-1,1),(-5,4),(-2,-5),(1,3),(4,-6),(-3,-6),(-7,-7),(3,-5),(2,-9),(-2,-9),(-4,9),(-1,-8),(-8,9),(0,-7),(-7,-3),(-6,-7),(-8,7),(7,-4),(-2,5),(-1,4),(7,5),(-1,8),(7,9),(0,-4),(-8,5),(2,0),(-1,7),(6,-5),(0,-3),(4,9),(4,5),(-2,-3),(4,2),(-3,2),(6,-3),(2,-1),(6,-1),(-8,-3),(4,-3),(2,7),(-6,-9),(6,1),(-1,-1),(-6,7),(-2,3),(1,-1),(-5,5),(7,-8),(0,4),(-8,-5),(-6,1),(-5,2),(-5,-7),(-1,3),(6,-7),(-6,-3),(-6,9),(-4,-9),(5,-9),(0,-8),(2,-6),(-5,-4),(0,-1),(-8,6),(-3,-9),(0,3),(6,7),(2,2),(5,9),(-8,-4),(-6,5),(2,9),(-8,3),(-3,0),(1,7),(7,-3),(1,-7),(-5,-1),(-1,-7),(7,-1),(-5,-6),(1,9),(2,6),(2,-5),(-1,5),(2,1),(4,3),(-2,7),(-1,-4),(8,1),(3,9),(3,-3),(6,-6),(-3,3),(4,7),(7,8),(1,-5),(-5,7),(-8,-8),(-3,5),(5,1),(0,7),(-3,-5),(-7,-6),(-7,3),(-8,8),(4,-7),(6,5),(0,5),(-5,9),(-3,6),(3,7),(7,-9),(0,-9),(6,9),(0,1),(2,3),(8,-1),(-8,1),(-5,0),(-8,-7),(7,3),(-7,5),(-3,1),(4,-5),(-1,-3),(4,6),(-3,-1),(7,6),(-9,1),(1,-3),(7,-7),(6,-9),(-1,-5),(-5,-5),(-8,-9),(-4,-3),(3,1),(2,-3),(-5,3),(5,-3),(-3,9),(6,3),(2,5),(4,-2),(-7,9),(1,5),(4,0),(5,-7),(-9,3),(-7,1),(5,7),(2,-2),(7,-5),(7,1),(-1,9),(8,3),(2,-7),(-7,-5),(4,8),(-5,-9),(-2,-1),(-5,1),(-5,8),(-9,-1)],[(-1,-4),(-2,5),(-3,-3),(-1,-8),(-8,-9),(-3,-9),(-8,-4),(-8,5),(-8,6),(-3,-6),(-8,-7),(-8,-8),(-7,-7),(-3,-7),(-1,-3),(-6,-3),(-7,-3),(-3,-5),(-1,5),(-2,-5),(-6,-9),(-6,-7),(-4,-3),(-5,3),(-7,-6),(-4,-5),(-1,-7),(-7,-9),(-8,-3),(-1,-5),(-4,-9),(-2,-3),(-1,8),(-7,-5),(-2,-9),(-2,-7),(-1,-9)],[(-3,3),(-8,-5)],[(7,-3),(2,-6),(6,-7),(7,-8),(7,6),(4,3),(2,-5),(0,-9),(0,-8),(0,-3),(3,-3),(2,-3),(0,8),(6,-9),(6,-3),(7,-7),(1,-7),(0,5),(3,-9),(3,-5),(2,-9),(7,-9),(7,-4),(0,-7),(1,5),(0,-5),(1,-9),(1,-5),(5,-7),(2,-7),(1,-3),(6,-5),(7,5),(0,-4),(6,-6),(5,-9),(5,-3)],[(7,-5),(2,3)],Decisao)


aStar_11( Inicial, Goal, Free, Sol ):-
	manhatan(Inicial, Goal, Distancia), 
	H is Distancia,
	aStar_aux_11( Inicial, Goal, Free, [ [H, 0, Inicial] ], [ ( H, Inicial ) ], 1, Sol ).

aStar_aux_11( _, Goal, _, [ [ _, C, (X, Y) | CurTree ] | _ ] ,_, _, Sol ):-
	manhatan((X, Y), Goal, Dist),
	Dist == 0,
	write('Custo '), write(C), nl,
	reverse( [(X, Y) | CurTree], Sol ).

aStar_aux_11( Inicial, Goal, Free, [ [ _, C, (X, Y) | CurTree ] | UpTree ] ,Visitados, Iteracao, Sol ):-
	sucs( (X, Y), Free, CurTree, Sucs ),
	Custo is (C + 1),
	applyHeuristic( Custo, Sucs, Goal, GenList ),
	write(GenList), write(' '), nl,
	append_all( GenList, [(X, Y) | CurTree], HeurList),
	write('GenList'), nl,
	write(HeurList), write(' '), nl,
	append(UpTree, HeurList, FinalList),
	write('FinaList'), nl,
	write(FinalList), nl,
	setof( [ H | T ], ( T^member( [H | T], FinalList ) ), Sorted ),
	aStar_aux_11( Inicial, Goal, Free, Sorted, Visitados, Iteracao, Sol ).
	%append_all( Pre, CurTree, GeneratedTree ),

%aStar_11( (-9,1), (8,1), [(-4,9),(-2,9),(0,-8),(8,1),(-8,1),(-6,5),(-8,-7),(0,5),(4,-9),(0,-5),(-5,-6),(2,-7),(-7,7),(0,-1),(-9,1),(-2,-3),(-8,6),(-1,1),(4,-4),(6,-5),(-3,-2),(0,-9),(-5,-9),(4,9),(4,-6),(2,-2),(7,9),(5,1),(2,6),(-5,-3),(8,-1),(2,-6),(-9,-1),(-3,9),(4,-1),(7,3),(-8,-1),(7,-9),(-7,-5),(-8,8),(1,-5),(4,-7),(-2,-5),(-4,-9),(5,-7),(-7,5),(6,-1),(2,-1),(5,-3),(-4,7),(-3,-5),(4,3),(0,-4),(-7,-6),(4,-5),(-6,-9),(-6,1),(-1,-1),(-4,1),(1,-3),(2,9),(0,7),(6,1),(-8,9),(4,1),(7,5),(-7,-1),(-3,5),(-3,1),(1,7),(-2,7),(1,3),(7,-3),(6,7),(-1,4),(-7,1),(0,-3),(-9,3),(-4,-5),(-5,-7),(2,-9),(4,8),(-5,6),(-2,3),(-8,5),(-7,3),(-8,-5),(1,-9),(4,5),(-5,8),(-1,-5),(-5,-4),(5,5),(-5,5),(0,9),(-1,-3),(7,-5),(-2,-9),(4,6),(-1,-9),(4,-3),(3,-9),(3,1),(-6,-7),(8,3),(5,-9),(-8,3),(5,7),(6,-7),(-5,-2),(7,7),(-1,-7),(-3,-9),(-8,-8),(-1,9),(7,-4),(2,0),(-5,-1),(-5,3),(7,6),(3,-3),(-3,-7),(6,-6),(-8,-9),(4,4),(7,1),(-4,-3),(2,-3),(-6,7),(-5,1),(-5,7),(-3,-3),(-3,-1),(1,9),(4,7),(-3,-6),(2,5),(2,7),(1,5),(7,-8),(2,-5),(7,-7),(0,4),(-7,-3),(0,3),(-6,-3),(-5,4),(-5,-5),(6,-3),(-3,0),(-1,-8),(4,-2),(-2,-1),(-1,7),(1,-1),(-8,-4),(-8,7),(0,-7),(-3,3),(-2,-7),(3,-5),(-3,6),(-7,-9),(-2,5),(2,3),(-1,3),(0,1),(2,1),(1,-7),(-5,2),(6,3),(-1,5),(3,9),(-7,9),(-3,2),(-6,9),(6,5),(2,2),(-3,7),(7,-1),(4,0),(-1,-4),(-8,-3),(-5,0),(-7,-7),(6,-9),(5,9),(3,7),(-1,8),(4,2),(0,8),(6,9),(-5,9),(7,8)], Sol ).

append_all( [], _, [] ).
append_all( [(H, C, (X,Y)) | R], L, Sol ):-
	append( [H, C, (X,Y)], L, This ),
	%write(This), write(' - '), nl,
	append_all( R, L, Rec ),
	Sol = [ This | Rec].

applyHeuristic( _, [], _, [] ).
applyHeuristic( Custo, [ (X, Y) | R ], Goal, Res ):-
	manhatan( (X, Y), Goal, Dist ),
	Heuristic is (Dist + Custo),
	applyHeuristic( Custo, R, Goal, Rec ),
	Res = [ (Heuristic, Custo, (X, Y)) | Rec ].


calcDistAll( _, [], [] ).
calcDistAll( Objectivo, [ Cur | R ], Res ):-
	manhatan( Cur, Objectivo, D ),
	M = (D, Cur),
	calcDistAll( Objectivo, R, Res2 ),
	Res = [M | Res2].


sucs( Cur, Free, CurTree, Sucs ):-
	findall( Suc , ( viz( _, Cur, Suc ), member( Suc, Free ), \+member( (_, Suc), CurTree ) ), Sucs).

viz(0,(X,Y),(X,NY)) :-
	NY is Y + 1.
viz(180,(X,Y),(X,NY)) :-
	NY is Y - 1.
viz(90,(X,Y),(NX,Y)) :-
	NX is X + 1.
viz(270,(X,Y),(NX,Y)) :-
	NX is X - 1.

manhatan( (X1, Y1), (X2, Y2), Dist ):-
	DistX is X1 - X2, abs(DistX, AbsX),
	DistY is Y1 - Y2, abs(DistY, AbsY),
	Dist is AbsX + AbsY.

concat_11(L1, L2, Res):-
	append(L1, L2, Res).


%--update nos visitados
update_visitados( [], Visitados, Visitados ).
update_visitados( [ (H, _, (X, Y)) | R ], Visitados, Res ):-
	member( (Hv, (X, Y)), Visitados ), Hv > H,
	replace_visitado( (H, (X, Y)), Visitados, [], NewVisitados ),
	write('Aqui 1'), nl,
	write(NewVisitados), nl,
	update_visitados( R, NewVisitados, Res ).

update_visitados( [ (H, _, (X, Y)) | R ], Visitados, Res ):-
	member( (Hv, (X, Y)), Visitados ), ( Hv < H ; Hv == H ),
	write('Aqui 2'), nl,
	update_visitados( R, Visitados, Res ).

update_visitados( [ (H, _, (X, Y)) | R ], Visitados, Res ):-
	\+member( (_, (X, Y)), Visitados ),
	append( ( H, (X, Y) ), Visitados, NewVisitados ),
	write('Aqui 3'), nl,
	update_visitados( R, NewVisitados, Res ).

replace_visitado( (H, (X, Y)), [ (Hv, (Xv, Yv)) | R ], Acc, Res ):-
	X == Xv, Y == Yv,
	H < Hv,
	append( [(H, (X, Y))], Acc, NewAcc ),
	Res = [ NewAcc | R ].
replace_visitado( (H, (X, Y)), [ (Hv, (Xv, Yv)) | R ], Acc, Res ):-
	(X \= Xv ; Y \= Yv),
	append( [(Hv, (Xv, Yv))], Acc, NewAcc ),
	replace_visitado( (H, (X, Y)), R, NewAcc, Res ).

%--update sucessores de acordo com visitados
update_sucs( [], _, Acc, Acc ).
update_sucs( [ (H, C, (X, Y)) | R ], Visitados, Acc, Res ):-
	( \+member( (_, (X, Y)), Visitados ) ; ( member( (Hv, (X, Y)), Visitados ), ( H < Hv ; H == Hv ) ) ),
	append( [(H, C, (X, Y))], Acc, NewAcc ),
	update_sucs(R, Visitados, NewAcc, Res).
update_sucs( [ (H, _, (X, Y)) | R ], Visitados, Acc, Res ):-
	member( (Hv, (X, Y)), Visitados ), H > Hv,
	update_sucs( R, Visitados, Acc, Res ).


