:- dynamic last_pos/2.

%sucessores
sucs( Cur, Free, CurTree, Sucs ):-
	findall( Suc , ( viz( _, Cur, Suc ), member( Suc, Free ), \+member( (_, Suc), CurTree ) ), Sucs).

%calcula distancia de manhatan
manhatan( (X1, Y1), (X2, Y2), Dist ):-
	DistX is X1 - X2, abs(DistX, AbsX),
	DistY is Y1 - Y2, abs(DistY, AbsY),
	Dist is AbsX + AbsY.

%calcular distancia de manhatan de n posicoes até um destino
calcDistAll( _, [], [] ).
calcDistAll( Objectivo, [ Cur | R ], Res ):-
	manhatan( Cur, Objectivo, D ),
	M = (D, Cur),
	calcDistAll( Objectivo, R, Res2 ),
	Res = [M | Res2].

%SORT
sort_by_custo( [], Res, Res ).
sort_by_custo( [ ( D, (X, Y) ) | R ], L, Res ):-
		insert_ordered( ( D, (X, Y) ), L, NL ),
		sort_by_custo( R, NL, Res ).
%INSERT_AUX_DE_SORT
insert_ordered( (Dist, (X, Y)), [], [(Dist, (X, Y))] ).
insert_ordered( (Dist, (X, Y)), [ (Dist2, (X2, Y2)) | R ], L ):-
	( Dist < Dist2 ; Dist == Dist2 ),
	L = [ (Dist, (X, Y)), (Dist2, (X2, Y2)) | R ].
insert_ordered( (Dist, (X, Y)), [ (Dist2, (X2, Y2)) | R ], L ):-
	Dist > Dist2,
	insert_ordered( (Dist, (X, Y)), R, NL ),
	L = [ (Dist2, (X2, Y2)) | NL ].

%criar sub arvores
appendAll( [], _, Acc, Acc ).
appendAll( [ Cur | R ], Tree, Acc, Res ):-
	append([Cur], Tree, SubTree),
	appendAll( R, Tree, [ SubTree | Acc ], Res ).


%obtem proxima direcao
%caso base
get_dir( pastilhas, N, (PacX, PacY), ( ObjX, ObjY ), [ [ (D, (X, Y)) | IR ] | _ ], _, Gums, Dec ):-
	%write('Cucu'), nl,
	%write(X), write(' : '), write(Y), nl,
	X =:= ObjX, Y =:= ObjY,
	%write(ObjX), write(' : '), write(ObjY), nl,
	%write('encontrou pastilha'), nl,
	reverse( [ (D, (X, Y)) | IR ], [Prev, (K, Xx, Yy) | Rev] ),
	%write('fez reverse'), nl,
	viz(Viz, (PacX, PacY), (Xx, Yy)),
	%write(Xx), write(' '), write(Yy), nl,
	%write('fez viz'), nl,
	%write([ (D, (X, Y)) | IR ]), nl, write(Rev), nl,
	Dec is Viz.

%explorer :P

	
get_dir( pastilhas, N, (PacX, PacY), ( ObjX, ObjY ), [ [ (D, (X, Y)) | IR ] | OR ], Free, Gums, Dec ):-
	%write('aiai'), nl,
	sucs( (X, Y), Free, [ (D, (X, Y)) | IR ], Sucs ),
	%write('piupiu'), nl,
	calcDistAll(( ObjX, ObjY ),Sucs, L ),
	%write('epaepah'), nl,
	sort_by_custo( L, [], SortedL ),
	%write('virgem'), nl,
	appendAll( SortedL, [ (D, (X, Y)) | IR ], [], SubTree ), reverse( SubTree, PSubTree ),
	append(PSubTree, OR, NL),
	NewN is N + 1,
	%write(NL), nl,
	%write('extra'), nl,
	get_dir( pastilhas,NewN, (PacX, PacY), ( ObjX, ObjY ), NL, Free, Gums, Dec ).

%pacman0(2,300,0,(0,-8,1,90,0),(1,-8,1,90,0),[(3,6,1,270,0),(2,6,1,270,0)],(-9,1),(8,1),[(-4,9),(-2,9),(0,-8),(8,1),(-8,1),(-6,5),(-8,-7),(0,5),(4,-9),(0,-5),(-5,-6),(2,-7),(-7,7),(0,-1),(-9,1),(-2,-3),(-8,6),(-1,1),(4,-4),(6,-5),(-3,-2),(0,-9),(-5,-9),(4,9),(4,-6),(2,-2),(7,9),(5,1),(2,6),(-5,-3),(8,-1),(2,-6),(-9,-1),(-3,9),(4,-1),(7,3),(-8,-1),(7,-9),(-7,-5),(-8,8),(1,-5),(4,-7),(-2,-5),(-4,-9),(5,-7),(-7,5),(6,-1),(2,-1),(5,-3),(-4,7),(-3,-5),(4,3),(0,-4),(-7,-6),(4,-5),(-6,-9),(-6,1),(-1,-1),(-4,1),(1,-3),(2,9),(0,7),(6,1),(-8,9),(4,1),(7,5),(-7,-1),(-3,5),(-3,1),(1,7),(-2,7),(1,3),(7,-3),(6,7),(-1,4),(-7,1),(0,-3),(-9,3),(-4,-5),(-5,-7),(2,-9),(4,8),(-5,6),(-2,3),(-8,5),(-7,3),(-8,-5),(1,-9),(4,5),(-5,8),(-1,-5),(-5,-4),(5,5),(-5,5),(0,9),(-1,-3),(7,-5),(-2,-9),(4,6),(-1,-9),(4,-3),(3,-9),(3,1),(-6,-7),(8,3),(5,-9),(-8,3),(5,7),(6,-7),(-5,-2),(7,7),(-1,-7),(-3,-9),(-8,-8),(-1,9),(7,-4),(2,0),(-5,-1),(-5,3),(7,6),(3,-3),(-3,-7),(6,-6),(-8,-9),(4,4),(7,1),(-4,-3),(2,-3),(-6,7),(-5,1),(-5,7),(-3,-3),(-3,-1),(1,9),(4,7),(-3,-6),(2,5),(2,7),(1,5),(7,-8),(2,-5),(7,-7),(0,4),(-7,-3),(0,3),(-6,-3),(-5,4),(-5,-5),(6,-3),(-3,0),(-1,-8),(4,-2),(-2,-1),(-1,7),(1,-1),(-8,-4),(-8,7),(0,-7),(-3,3),(-2,-7),(3,-5),(-3,6),(-7,-9),(-2,5),(2,3),(-1,3),(0,1),(2,1),(1,-7),(-5,2),(6,3),(-1,5),(3,9),(-7,9),(-3,2),(-6,9),(6,5),(2,2),(-3,7),(7,-1),(4,0),(-1,-4),(-8,-3),(-5,0),(-7,-7),(6,-9),(5,9),(3,7),(-1,8),(4,2),(0,8),(6,9),(-5,9),(7,8)],[(-8,-8),(-4,-5),(-7,-9),(-5,3),(-6,5),(-5,2),(-7,-5),(-2,-7),(-6,9),(-3,7),(-1,9),(-3,-9),(-1,5),(-5,-1),(-5,9),(-5,-4),(-4,-3),(-5,-6),(-8,7),(-2,-3),(-1,7),(-1,8),(-7,-6),(-5,6),(-5,-7),(-3,-6),(-6,-3),(-5,1),(-5,-9),(-4,7),(-8,6),(-5,8),(-2,9),(-5,0),(-5,-3),(-8,-9),(-7,-3),(-8,-7),(-1,-5),(-1,-3),(-8,-3),(-3,-7),(-5,-5),(-8,-4),(-1,-7),(-4,-9),(-6,-9),(-8,5),(-1,-9),(-7,-7),(-6,7),(-8,9),(-3,9),(-3,5),(-7,7),(-1,-4),(-1,-8),(-3,6),(-7,9),(-6,-7),(-5,-2),(-2,-5),(-2,5),(-5,7),(-7,5),(-5,4),(-2,7),(-2,-9),(-3,-3),(-3,-5),(-5,5),(-4,9)],[(-8,8),(-8,-5)],[(6,5),(7,-7),(2,-6),(7,9),(4,8),(0,-9),(2,9),(3,-3),(7,-8),(0,7),(3,7),(4,3),(4,9),(6,9),(5,-9),(0,-7),(4,7),(7,7),(1,-3),(1,5),(1,-9),(4,2),(0,8),(3,-5),(3,9),(3,-9),(4,-7),(6,7),(7,5),(4,5),(1,9),(5,5),(0,-8),(4,-2),(4,-3),(6,-9),(4,6),(4,1),(2,-3),(1,-5),(1,-7),(4,-4),(5,-7),(2,6),(4,0),(7,-9),(4,-1),(0,-3),(4,-6),(4,-9),(6,-5),(0,-5),(7,-3),(2,-9),(5,9),(1,7),(2,-7),(4,-5),(5,-3),(2,5),(0,9),(6,-3),(0,5),(7,-4),(5,7),(6,-7),(0,-4),(7,6),(2,-5),(4,4),(6,-6),(2,7)],[(7,-5),(7,8)],Decisao)
%pacman22(12,300,0,(0,0,3,90,0),(1,-3,3,0,0),[(2,5,1,270,0),(3,2,0,180,0)],(-9,1),(8,1),[(4,-6),(-2,7),(-3,-7),(2,2),(3,1),(7,-8),(-1,8),(6,-7),(7,9),(-8,-1),(6,7),(-4,9),(2,-1),(5,1),(-1,-4),(0,4),(2,-7),(-8,3),(-2,5),(1,-7),(-8,1),(-5,7),(-5,-3),(-8,-8),(6,-1),(-7,1),(2,-9),(3,7),(-5,8),(7,-7),(2,6),(-8,-4),(-3,7),(-1,4),(-7,3),(-8,9),(-3,5),(0,-3),(4,-7),(7,7),(-1,1),(3,-5),(6,3),(4,6),(-6,5),(0,-7),(-5,-7),(4,1),(3,9),(0,1),(4,7),(8,-1),(-1,-5),(4,-1),(-6,7),(-5,5),(6,-3),(-8,5),(-8,-7),(-2,9),(-8,-9),(-3,-5),(-4,1),(0,-8),(4,-2),(4,-4),(-5,1),(-2,-5),(-4,-9),(-1,-3),(2,9),(-5,9),(1,3),(7,3),(-8,7),(-1,7),(7,-3),(-9,-1),(-3,9),(-3,-2),(7,-1),(0,-5),(4,0),(-3,-3),(2,1),(-5,-6),(0,-4),(-2,-1),(-7,-1),(-4,-3),(0,7),(-1,9),(-3,6),(-7,-9),(2,0),(-8,8),(5,5),(-7,7),(-3,1),(-1,-9),(4,-9),(1,9),(-5,0),(-7,-6),(5,-7),(3,-3),(-9,3),(3,-9),(-6,9),(6,5),(-2,-9),(2,-5),(0,8),(7,8),(1,-9),(-3,-9),(4,-3),(-4,-5),(6,-6),(5,-9),(0,5),(-3,-1),(2,3),(-1,3),(-6,-7),(5,9),(-7,-7),(6,1),(-7,-3),(4,5),(8,1),(-1,-8),(1,-5),(-8,-5),(0,-1),(6,-5),(6,9),(-4,7),(-3,0),(7,1),(7,5),(2,5),(-3,3),(4,-5),(-8,6),(-5,-9),(-7,5),(-5,-2),(5,-3),(-5,2),(-5,6),(-2,3),(-6,-9),(7,6),(0,-9),(7,-4),(1,-3),(1,5),(-7,-5),(0,3),(4,3),(0,9),(1,7),(8,3),(-5,4),(4,8),(-5,-4),(2,7),(7,-9),(2,-2),(-1,-7),(-6,1),(-3,-6),(-5,-5),(-3,2),(4,9),(-9,1),(-2,-7),(1,-1),(-8,-3),(6,-9),(2,-6),(-5,3),(2,-3),(-7,9),(7,-5),(-1,-1),(5,7),(4,4),(4,2),(-2,-3),(-5,-1),(-1,5),(-6,-3)],[(-5,-4),(-5,-3),(-8,6),(-6,-9),(-7,-5),(-4,9),(-3,-7),(-1,9),(-8,-9),(-3,9),(-1,-4),(-6,5),(-5,8),(-1,-8),(-5,-9),(-5,6),(-2,-5),(-8,-8),(-8,-7),(-4,-5),(-7,9),(-7,5),(-3,6),(-6,-3),(-1,8),(-5,-6),(-8,-4),(-1,-3),(-3,-5),(-8,-3),(-5,-2),(-1,-7),(-5,7),(-4,7),(-7,-9),(-5,4),(-2,5),(-7,-7),(-6,9),(-2,-7),(-1,5),(-5,-5),(-5,-7),(-5,2),(-5,5),(-2,7),(-5,3),(-8,9),(-5,0),(-3,-3),(-3,5),(-8,5),(-4,-9),(-6,-7),(-7,-3),(-7,-6),(-1,-5),(-5,9),(-3,-9),(-5,1),(-5,-1),(-7,7),(-1,7),(-2,-3),(-2,9),(-4,-3),(-2,-9),(-1,-9),(-3,-6),(-8,7),(-3,7),(-6,7)],[(-8,-5),(-8,8)],[(7,-8),(5,9),(1,-9),(6,-5),(6,7),(2,9),(0,9),(4,-4),(1,-5),(4,6),(2,6),(4,4),(4,-7),(4,2),(7,-4),(4,3),(2,-6),(0,-3),(6,9),(0,-4),(7,5),(2,5),(6,5),(4,-3),(2,-5),(7,-7),(2,-7),(4,-1),(7,9),(4,-5),(6,-6),(3,-5),(1,5),(4,5),(4,0),(2,7),(3,-9),(1,7),(7,6),(5,-3),(5,7),(3,9),(3,-3),(0,-9),(5,-7),(7,7),(1,9),(7,-9),(6,-9),(0,-5),(4,1),(0,7),(4,-2),(0,-8),(4,-6),(2,-9),(0,8),(4,-9),(1,-3),(2,-3),(3,7),(1,-7),(7,-3),(5,5),(6,-7),(4,8),(5,-9),(4,7),(6,-3),(0,-7),(4,9),(0,5)],[(7,-5),(7,8)],Decisao)
%pacman(Clock, ClockLimit, Score, Me, Partner, OtherTeam, HomeBase , HisBase, FreeCells, MyP, MYSupP, HisP, HisSupP, Decisao) :-
%pacman22(206,300,42,(1,4,5,90,0),(0,2,3,90,0),[(3,4,6,180,0),(2,-3,-5,0,0)],(-9,1),(8,1),[(-4,-3),(3,-5),(7,-5),(-1,-7),(5,-9),(4,-4),(-3,-3),(-2,5),(-5,4),(0,-7),(-5,5),(4,-3),(-7,-5),(-5,-6),(-3,-7),(2,7),(6,-6),(-3,-9),(-5,-4),(0,4),(-1,-8),(-3,5),(-2,-1),(7,-3),(-1,-4),(1,3),(2,1),(2,-9),(-5,6),(6,3),(4,7),(-8,7),(-5,3),(-7,-1),(6,-7),(4,-9),(-2,-3),(-8,5),(-3,-6),(6,7),(-1,7),(0,1),(-7,-7),(-7,5),(5,-7),(1,9),(1,-3),(4,-1),(-5,2),(7,9),(2,-2),(-9,1),(2,0),(-5,-2),(-6,1),(8,1),(6,5),(-5,8),(-3,3),(-3,2),(-6,-9),(2,9),(-7,1),(-7,-9),(-1,8),(-8,-4),(-9,3),(-1,-1),(2,-7),(2,-6),(-1,9),(-1,-9),(2,5),(4,0),(3,-9),(-1,5),(7,-7),(2,3),(-6,-3),(4,8),(1,-9),(2,-5),(0,-8),(1,-5),(-7,7),(3,1),(2,-3),(-1,3),(-8,-8),(-4,7),(0,5),(4,-7),(-3,7),(-3,9),(-2,-9),(-3,1),(5,7),(6,-9),(6,-5),(0,-3),(-2,-7),(-4,9),(-1,-3),(1,7),(-8,-7),(-6,5),(-5,0),(3,7),(-5,7),(3,9),(-5,9),(0,7),(-6,7),(-8,9),(-8,-5),(5,1),(-5,1),(-6,9),(0,-9),(-7,9),(0,-5),(-8,-3),(6,-1),(6,9),(7,-1),(-3,-2),(-4,-5),(7,-8),(-3,0),(7,6),(-2,9),(1,5),(-5,-3),(-7,-3),(-8,-1),(7,3),(0,-1),(2,-1),(-7,3),(5,5),(-9,-1),(0,8),(7,-4),(4,3),(4,9),(-3,6),(7,5),(-2,-5),(-1,4),(5,9),(6,-3),(8,3),(0,3),(7,8),(-8,8),(4,-2),(-4,-9),(2,6),(-2,7),(5,-3),(-8,1),(-3,-5),(-1,1),(-3,-1),(-2,3),(-8,6),(7,7),(-8,3),(-5,-5),(-5,-7),(3,-3),(0,-4),(0,9),(4,2),(6,1),(8,-1),(7,-9),(-5,-1),(4,4),(4,-6),(4,6),(4,1),(-6,-7),(4,5),(1,-1),(4,-5),(2,2),(7,1),(-4,1),(1,-7),(-5,-9),(-8,-9),(-1,-5),(-7,-6)],[(-6,7),(-5,-1),(-8,-7),(-8,-3),(-8,7),(-5,-3),(-8,-8),(-7,-9),(-5,5),(-3,6),(-6,-9),(-2,-9),(-7,-7),(-1,-8),(-5,1),(-7,7),(-4,-9),(-7,9),(-1,-9),(-5,-2),(-6,9),(-4,-3),(-7,-3),(-5,2),(-1,-7),(-5,6),(-7,-6),(-6,-3),(-6,5),(-5,-4),(-5,0),(-8,9),(-8,-4),(-2,-7),(-8,6),(-5,3),(-5,4),(-8,-9),(-8,5),(-7,5),(-5,-9),(-7,-5),(-6,-7),(-3,-9)],[(-8,-5),(-8,8)],[(5,9),(6,9),(7,9)],[(7,8)],Decisao)

pacman22(_,_,_,(_,PacX,PacY,PacDir,_),_,_,_,_,Free,_,_,Pastilhas,MaxPastilhas,Dec) :-
	calcDistAll( (PacX, PacY), Pastilhas, ListaDistPastilhas ),
	sort_by_custo( ListaDistPastilhas, [], [ (Dist, ( ObjX, ObjY ) ) | R ] ),
	get_dir( pastilhas,0, (PacX, PacY), (ObjX, ObjY), [[ (0, (PacX, PacY)) ]], Free, Pastilhas, Dec ).

viz(0,(X,Y),(X,NY)) :-
	NY is Y + 1.
viz(180,(X,Y),(X,NY)) :-
	NY is Y - 1.
viz(90,(X,Y),(NX,Y)) :-
	NX is X + 1.
viz(270,(X,Y),(NX,Y)) :-
	NX is X - 1.


%[(-8,6),(-5,7),(-5,2),(-7,9),(-8,-9),(-7,-3),(-4,-3),(-5,4),(-5,0),(-2,-7),(-4,7),(-7,7),(-4,9),(-3,-3),(-2,9),(-1,8),(-5,-7),(-5,-6),(-5,8),(-3,9),(-3,-6),(-5,-2),(-3,-9),(-5,-9),(-1,-3),(-2,-9),(-1,-4),(-5,-1),(-6,-7),(-2,5),(-8,9),(-8,-4),(-2,-3),(-5,6),(-1,9),(-5,9),(-5,-3),(-1,-8),(-8,5),(-4,-5),(-6,-3),(-5,-4),(-1,-5),(-3,-7),(-2,-5),(-7,5),(-1,7),(-8,-8),(-7,-9),(-3,7),(-8,7),(-6,7),(-6,-9),(-6,9),(-5,-5),(-8,-3),(-1,5),(-7,-7),(-2,7),(-1,-7),(-4,-9),(-3,6),(-1,-9),(-5,3),(-5,1),(-3,-5),(-5,5),(-6,5),(-8,-7),(-3,5),(-7,-6),(-7,-5)]