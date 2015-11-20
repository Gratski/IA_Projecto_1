%split_List_By_Parity( Value, L, R ):-
%	0 =:= mod(Value, 2),
	%faz bottom.

%split_List_By_Parity( Value, L, R ):-
%	1 =:= mod(Value, 2),
	%faz top.

%%-- PACMAN %
pacman19(Id,_,_,(_,PacX,PacY,PacDir,_),_,_,_,_,Free,_,_,Pastilhas,MaxPastilhas,Dec):-
	concat_11(Pastilhas, MaxPastilhas, AllPastilhas),
	calcDistAll( (PacX, PacY), AllPastilhas, Distancias ),
	setof( (D, (X, Y)), ( X^Y^member( (D, (X, Y)), Distancias ) ), [ (D1, (X1, Y1)) | RorderdDistancias] ),
	my_include(D1, [ (D1, (X1, Y1)) | RorderdDistancias], PTargets),
	devolve_n_primeiros( 3, PTargets, Targets ),
	applyNivel( 3, Targets, AllPastilhas, Calculated ),
	setof( (Num, (NumX, NumY)), ( NumX^NumY^member( (Num, (NumX, NumY)), Calculated ) ), PInterest ),
	reverse( PInterest, [ (_, (Px, Py)) | _ ] ),
	aStar_11( (PacX, PacY), (Px, Py), Free, [ _,(PinX, PinY) | PinR] ),
	viz_19(Dec, (PacX, PacY), (PinX, PinY)).


%%-- ALGORITMO DE PESQUISA DE CAMINHO OPTIMO %
aStar_11( Inicial, Goal, Free, Sol ):-
	write('---'), nl,
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
	%write(GenList), write(' '), nl,
	append_all( GenList, [(X, Y) | CurTree], HeurList),
	%write('GenList'), nl,
	%write(HeurList), write(' '), nl,
	append(UpTree, HeurList, FinalList),
	%write('FinaList'), nl,
	%write(FinalList), nl,
	setof( [ H | T ], ( T^member( [H | T], FinalList ) ), Sorted ),
	aStar_aux_11( Inicial, Goal, Free, Sorted, Visitados, Iteracao, Sol ).


%%-- FUNCAO DE HEURISTICA %
applyHeuristic( _, [], _, [] ).
applyHeuristic( Custo, [ (X, Y) | R ], Goal, Res ):-
	manhatan( (X, Y), Goal, Dist ),
	Heuristic is (Dist + Custo),
	applyHeuristic( Custo, R, Goal, Rec ),
	Res = [ (Heuristic, Custo, (X, Y)) | Rec ].

%%-- SUCESSORES DE UM NO %
sucs( Cur, Free, CurTree, Sucs ):-
	findall( Suc , ( viz_19( _, Cur, Suc ), member( Suc, Free ), \+member( (_, Suc), CurTree ) ), Sucs).

%%-- VIZINHO %
viz(0,(_,_),(_,_)).
viz_19(0,(X,Y),(X,NY)) :-
	NY is Y + 1.
viz_19(180,(X,Y),(X,NY)) :-
	NY is Y - 1.
viz_19(90,(X,Y),(NX,Y)) :-
	NX is X + 1.
viz_19(270,(X,Y),(NX,Y)) :-
	NX is X - 1.

%%-- AVALIACAO DE VIZINHANCA %
applyNivel(_, [], _, []).
applyNivel(N, [ (X, Y) | R ], Targets, Res ):-
	nivel( 0, [(X, Y)], Targets, [(X, Y)], Nivel ),
	applyNivel( N, R, Targets, Rec ),
	Res = [ (Nivel, (X, Y)) | Rec ].

nivel( _, [], _, _, 0 ).
nivel( 3, [(X, Y) | R], Pastilhas, Visitados, Res ):-
	write('Limite: '), write([(X, Y) | R]), nl,
	nivel( 2, R, Pastilhas, Visitados, Rec ),
	Res is 1 + Rec.
nivel( Depth, [ (X, Y) | R ], Pastilhas, Visitados, Res ):-
	write('Explorer: '), write([(X, Y) | R]), nl,
	findall( (Xx, Yy), ( viz_19( _, (X, Y), (Xx, Yy) ), member( (Xx, Yy), Pastilhas ), \+member( (Xx, Yy), Visitados ) ), Sucs ),
	append( Sucs, Visitados, NewVisitados ),
	append( Sucs, R, NewTree ),
	NDepth is Depth + 1,
	nivel( NDepth, NewTree, Pastilhas, NewVisitados, Rec ),
	Res is 1 + Rec.

%%-- MANIPULACAO DE LISTAS %
devolve_n_primeiros( _, [], [] ).
devolve_n_primeiros( N, _, [] ):-
	N < 1.
devolve_n_primeiros( N, [ (_, (X, Y)) | R ], Res ):-
	NewN is N - 1,
	devolve_n_primeiros( NewN, R, Rec ),
	Res = [ (X, Y) | Rec ].

concat_11(L1, L2, Res):-
	append(L1, L2, Res).

append_all( [], _, [] ).
append_all( [(H, C, (X,Y)) | R], L, Sol ):-
	append( [H, C, (X,Y)], L, This ),
	%write(This), write(' - '), nl,
	append_all( R, L, Rec ),
	Sol = [ This | Rec].

my_include( _, [], [] ).
my_include( V, [(D, (_, _)) | _], [] ):-
		V \= D.
my_include( V, [(D, (X, Y)) | R], Res ):-
	V == D,
	my_include( V, R, Rec),
	Res = [ (D, (X, Y)) | Rec ].

%%-- CALCULO DE DISTANCIAS %
calcDistAll( _, [], [] ).
calcDistAll( Objectivo, [ Cur | R ], Res ):-
	manhatan( Cur, Objectivo, D ),
	M = (D, Cur),
	calcDistAll( Objectivo, R, Res2 ),
	Res = [M | Res2].

manhatan( (X1, Y1), (X2, Y2), Dist ):-
	DistX is X1 - X2, abs(DistX, AbsX),
	DistY is Y1 - Y2, abs(DistY, AbsY),
	Dist is AbsX + AbsY.


%%-- CHAMADAS PARA TESTES %
%nivel(0, [(7, -3)], [(6,-9),(5,-7),(7,-3),(3,-3),(6,-5),(7,-7),(6,-3),(2,-3),(2,-9),(2,-6),(3,-9),(7,-8),(0,-3),(6,-6),(2,-5),(0,5),(6,-7),(0,-5),(1,5),(7,-9),(7,6),(3,-5),(0,8),(4,3),(0,-4),(0,-9),(7,-4),(5,-3),(1,-7),(7,5),(1,-9),(1,-3),(1,-5),(0,-8),(2,-7),(0,-7),(5,-9)], [(7,-3)], Pastilhas).
%pacman19(2,300,0,(0,-8,1,90,0),(1,-8,1,90,0),[(3,6,1,270,0),(2,6,1,270,0)],(-9,1),(8,1),[(-4,9),(-2,9),(0,-8),(8,1),(-8,1),(-6,5),(-8,-7),(0,5),(4,-9),(0,-5),(-5,-6),(2,-7),(-7,7),(0,-1),(-9,1),(-2,-3),(-8,6),(-1,1),(4,-4),(6,-5),(-3,-2),(0,-9),(-5,-9),(4,9),(4,-6),(2,-2),(7,9),(5,1),(2,6),(-5,-3),(8,-1),(2,-6),(-9,-1),(-3,9),(4,-1),(7,3),(-8,-1),(7,-9),(-7,-5),(-8,8),(1,-5),(4,-7),(-2,-5),(-4,-9),(5,-7),(-7,5),(6,-1),(2,-1),(5,-3),(-4,7),(-3,-5),(4,3),(0,-4),(-7,-6),(4,-5),(-6,-9),(-6,1),(-1,-1),(-4,1),(1,-3),(2,9),(0,7),(6,1),(-8,9),(4,1),(7,5),(-7,-1),(-3,5),(-3,1),(1,7),(-2,7),(1,3),(7,-3),(6,7),(-1,4),(-7,1),(0,-3),(-9,3),(-4,-5),(-5,-7),(2,-9),(4,8),(-5,6),(-2,3),(-8,5),(-7,3),(-8,-5),(1,-9),(4,5),(-5,8),(-1,-5),(-5,-4),(5,5),(-5,5),(0,9),(-1,-3),(7,-5),(-2,-9),(4,6),(-1,-9),(4,-3),(3,-9),(3,1),(-6,-7),(8,3),(5,-9),(-8,3),(5,7),(6,-7),(-5,-2),(7,7),(-1,-7),(-3,-9),(-8,-8),(-1,9),(7,-4),(2,0),(-5,-1),(-5,3),(7,6),(3,-3),(-3,-7),(6,-6),(-8,-9),(4,4),(7,1),(-4,-3),(2,-3),(-6,7),(-5,1),(-5,7),(-3,-3),(-3,-1),(1,9),(4,7),(-3,-6),(2,5),(2,7),(1,5),(7,-8),(2,-5),(7,-7),(0,4),(-7,-3),(0,3),(-6,-3),(-5,4),(-5,-5),(6,-3),(-3,0),(-1,-8),(4,-2),(-2,-1),(-1,7),(1,-1),(-8,-4),(-8,7),(0,-7),(-3,3),(-2,-7),(3,-5),(-3,6),(-7,-9),(-2,5),(2,3),(-1,3),(0,1),(2,1),(1,-7),(-5,2),(6,3),(-1,5),(3,9),(-7,9),(-3,2),(-6,9),(6,5),(2,2),(-3,7),(7,-1),(4,0),(-1,-4),(-8,-3),(-5,0),(-7,-7),(6,-9),(5,9),(3,7),(-1,8),(4,2),(0,8),(6,9),(-5,9),(7,8)],[(-8,-8),(-4,-5),(-7,-9),(-5,3),(-6,5),(-5,2),(-7,-5),(-2,-7),(-6,9),(-3,7),(-1,9),(-3,-9),(-1,5),(-5,-1),(-5,9),(-5,-4),(-4,-3),(-5,-6),(-8,7),(-2,-3),(-1,7),(-1,8),(-7,-6),(-5,6),(-5,-7),(-3,-6),(-6,-3),(-5,1),(-5,-9),(-4,7),(-8,6),(-5,8),(-2,9),(-5,0),(-5,-3),(-8,-9),(-7,-3),(-8,-7),(-1,-5),(-1,-3),(-8,-3),(-3,-7),(-5,-5),(-8,-4),(-1,-7),(-4,-9),(-6,-9),(-8,5),(-1,-9),(-7,-7),(-6,7),(-8,9),(-3,9),(-3,5),(-7,7),(-1,-4),(-1,-8),(-3,6),(-7,9),(-6,-7),(-5,-2),(-2,-5),(-2,5),(-5,7),(-7,5),(-5,4),(-2,7),(-2,-9),(-3,-3),(-3,-5),(-5,5),(-4,9)],[(-8,8),(-8,-5)],[(6,5),(7,-7),(2,-6),(7,9),(4,8),(0,-9),(2,9),(3,-3),(7,-8),(0,7),(3,7),(4,3),(4,9),(6,9),(5,-9),(0,-7),(4,7),(7,7),(1,-3),(1,5),(1,-9),(4,2),(0,8),(3,-5),(3,9),(3,-9),(4,-7),(6,7),(7,5),(4,5),(1,9),(5,5),(0,-8),(4,-2),(4,-3),(6,-9),(4,6),(4,1),(2,-3),(1,-5),(1,-7),(4,-4),(5,-7),(2,6),(4,0),(7,-9),(4,-1),(0,-3),(4,-6),(4,-9),(6,-5),(0,-5),(7,-3),(2,-9),(5,9),(1,7),(2,-7),(4,-5),(5,-3),(2,5),(0,9),(6,-3),(0,5),(7,-4),(5,7),(6,-7),(0,-4),(7,6),(2,-5),(4,4),(6,-6),(2,7)],[(7,-5),(7,8)],Decisao)
%Gums [(-4,9),(-2,9),(0,-8),(8,1),(-8,1),(-6,5),(-8,-7),(0,5),(4,-9),(0,-5),(-5,-6),(2,-7),(-7,7),(0,-1),(-9,1),(-2,-3),(-8,6),(-1,1),(4,-4),(6,-5),(-3,-2),(0,-9),(-5,-9),(4,9),(4,-6),(2,-2),(7,9),(5,1),(2,6),(-5,-3),(8,-1),(2,-6),(-9,-1),(-3,9),(4,-1),(7,3),(-8,-1),(7,-9),(-7,-5),(-8,8),(1,-5),(4,-7),(-2,-5),(-4,-9),(5,-7),(-7,5),(6,-1),(2,-1),(5,-3),(-4,7),(-3,-5),(4,3),(0,-4),(-7,-6),(4,-5),(-6,-9),(-6,1),(-1,-1),(-4,1),(1,-3),(2,9),(0,7),(6,1),(-8,9),(4,1),(7,5),(-7,-1),(-3,5),(-3,1),(1,7),(-2,7),(1,3),(7,-3),(6,7),(-1,4),(-7,1),(0,-3),(-9,3),(-4,-5),(-5,-7),(2,-9),(4,8),(-5,6),(-2,3),(-8,5),(-7,3),(-8,-5),(1,-9),(4,5),(-5,8),(-1,-5),(-5,-4),(5,5),(-5,5),(0,9),(-1,-3),(7,-5),(-2,-9),(4,6),(-1,-9),(4,-3),(3,-9),(3,1),(-6,-7),(8,3),(5,-9),(-8,3),(5,7),(6,-7),(-5,-2),(7,7),(-1,-7),(-3,-9),(-8,-8),(-1,9),(7,-4),(2,0),(-5,-1),(-5,3),(7,6),(3,-3),(-3,-7),(6,-6),(-8,-9),(4,4),(7,1),(-4,-3),(2,-3),(-6,7),(-5,1),(-5,7),(-3,-3),(-3,-1),(1,9),(4,7),(-3,-6),(2,5),(2,7),(1,5),(7,-8),(2,-5),(7,-7),(0,4),(-7,-3),(0,3),(-6,-3),(-5,4),(-5,-5),(6,-3),(-3,0),(-1,-8),(4,-2),(-2,-1),(-1,7),(1,-1),(-8,-4),(-8,7),(0,-7),(-3,3),(-2,-7),(3,-5),(-3,6),(-7,-9),(-2,5),(2,3),(-1,3),(0,1),(2,1),(1,-7),(-5,2),(6,3),(-1,5),(3,9),(-7,9),(-3,2),(-6,9),(6,5),(2,2),(-3,7),(7,-1),(4,0),(-1,-4),(-8,-3),(-5,0),(-7,-7),(6,-9),(5,9),(3,7),(-1,8),(4,2),(0,8),(6,9),(-5,9),(7,8)]
%pacman19(1,300,0,(1,4,-1,90,0),(0,-9,9,90,0),[(2,8,9,270,0),(3,8,9,270,0)],(-9,9),(8,9),[(-5,-1),(-6,5),(7,-8),(-5,-5),(-1,-9),(6,1),(4,-5),(-8,-5),(-1,-5),(2,9),(-3,0),(0,-3),(-3,6),(5,-9),(-8,9),(-8,-4),(0,-8),(4,-9),(2,-1),(6,5),(2,-2),(4,-6),(7,-9),(1,-7),(2,-7),(-5,0),(-9,3),(7,3),(-2,7),(4,2),(-5,-6),(4,-4),(4,0),(-5,4),(2,-9),(4,-1),(0,-7),(4,-3),(-1,4),(5,5),(3,7),(-8,-3),(-6,1),(4,8),(-2,-9),(0,5),(-3,-1),(2,0),(-9,9),(-1,-4),(-5,8),(-1,3),(-2,5),(-3,3),(6,-9),(0,-4),(2,1),(-4,-9),(-5,1),(-4,1),(-3,-7),(-7,-5),(-5,-9),(2,7),(8,9),(0,4),(-8,7),(-3,-5),(2,3),(1,-5),(7,-1),(7,-3),(-5,7),(-2,-7),(-1,1),(4,5),(0,-5),(-5,3),(1,-9),(6,7),(-9,-9),(-7,-1),(5,9),(-3,5),(3,-3),(2,5),(-9,-1),(7,-4),(-4,-5),(2,-3),(5,-3),(0,1),(-2,-3),(5,7),(-8,5),(1,-3),(-1,-1),(-2,3),(-5,-4),(-8,8),(6,-7),(-3,-6),(2,2),(0,8),(-7,9),(4,-7),(8,-1),(0,-1),(-8,-7),(-8,3),(2,-6),(-4,-3),(4,6),(-3,2),(0,9),(-7,-6),(3,9),(-2,9),(-3,-9),(4,4),(8,-9),(-8,6),(-7,5),(-1,8),(5,-7),(-3,7),(-2,-1),(-5,6),(0,-9),(-3,-3),(7,-5),(6,-6),(1,-1),(-9,1),(-4,9),(6,9),(-3,-2),(-7,3),(-1,7),(1,9),(4,-2),(-1,-8),(7,6),(7,7),(8,1),(-5,-7),(-2,-5),(-5,-3),(-8,-8),(-7,-9),(4,2),(-1,9),(8,3),(-8,-9),(0,3),(-7,-3),(1,7),(6,-5),(-6,-7),(7,9),(6,-3),(-4,7),(-5,2),(-8,-1),(-7,1),(-8,1),(-3,1),(-6,-9),(6,3),(4,1),(-7,7),(-5,9),(-5,-2),(-7,-7),(7,-7),(4,7),(-3,9),(-1,-7),(0,7),(-6,9),(-1,-3),(-5,5),(6,-1),(7,8),(2,-5),(5,1),(2,6),(-1,5),(1,3),(-6,7),(7,1),(4,9),(3,1),(3,-9),(-6,-3),(3,-5),(7,5),(1,5)],[(-8,-7),(-4,-3),(-3,-6),(-2,-7),(-8,-4),(-2,-5),(-3,-7),(-8,-8),(-2,-9),(-7,-7),(-1,-9),(-1,-3),(-2,-3),(-4,-5),(-3,-5),(-3,-9),(-4,-9),(-1,-5),(-1,-8),(-1,-7),(-8,-9),(-8,6),(-2,5),(-6,-7),(-7,-5),(-6,-3),(-8,5),(-7,-9),(-1,-4),(-7,-3),(-8,-3),(-7,-6),(-1,8),(-3,-3),(-6,-9),(-5,3),(-1,5)],[(-8,-5),(-3,3)],[(6,-9),(5,-7),(7,-3),(3,-3),(6,-5),(7,-7),(6,-3),(2,-3),(2,-9),(2,-6),(3,-9),(7,-8),(0,-3),(6,-6),(2,-5),(0,5),(6,-7),(0,-5),(1,5),(7,-9),(7,6),(3,-5),(0,8),(4,3),(0,-4),(0,-9),(7,-4),(5,-3),(1,-7),(7,5),(1,-9),(1,-3),(1,-5),(0,-8),(2,-7),(0,-7),(5,-9)],[(2,3),(7,-5)],Decisao)
