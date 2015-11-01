%sucessores
sucs( Cur, Free, Sucs ):-
	findall( Suc , ( viz( _, Cur, Suc ), member( Suc, Free ) ), Sucs).

%calcula distancia de manhatan
manhatan( (X1, Y1), (X2, Y2), Dist ):-
	DistX is X1 - X2, abs(DistX, AbsX),
	DistY is Y1 - Y2, abs(DistY, AbsY),
	Dist is AbsX + AbsY.

%calcular distancia de manhatan de n posicoes até um destino
calcDistAll( _, [], Acc, Acc ).
calcDistAll( Objectivo, [ Cur | R ], Acc, Res ):-
	manhatan( Cur, Objectivo, D ),
	M = (D, Cur),
	calcDistAll( Objectivo, R, [ M | Acc ], Res ).

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
	append(Cur, Tree, SubTree),
	appendAll( R, Tree, [ SubTree | Acc ], Res ).


%obtem proxima direcao
%caso base
get_dir( pastilhas, (PacX, PacY), _, [ [ (D, (X, Y)) | IR ] | _ ], _, Gums, Dec ):-
	member( (X, Y), Gums ),
	reverse( [ (D, (X, Y)) | IR ], [ (_ ,( HeadX, HeadY )) | _ ] ),
	viz(Dec, (PacX, PacY), (HeadX, HeadY)).

%explorer :P
get_dir( pastilhas, (PacX, PacY), ( ObjX, ObjY ), [ [ (D, (X, Y)) | IR ] | OR ], Free, Gums, Dec ):-
	sucs( (X, Y), Free, Sucs ),
	calcDistAll( Sucs, ( ObjX, ObjY ), [], L ), 
	sort_by_custo( L, [], SortedL ),
	appendAll( SortedL, [ (D, (X, Y)) | IR ], [], SubTree ),
	get_dir( pastilhas, (PacX, PacY), ( ObjX, ObjY ), [ SubTree | OR ], Free, Gums, Dec ).



%pacman(Clock, ClockLimit, Score, Me, Partner, OtherTeam, HomeBase , HisBase, FreeCells, MyP, MYSupP, HisP, HisSupP, Decisao) :-

pacman0(_,_,_,(_,PacX,PacY,PacDir,_),_,_,_,_,Free,_,_,Pastilhas,MaxPastilhas,Dec) :-
	calcDistAll( (PacX, PacY), Pastilhas, ListaDistPastilhas ),
	sort_by_custo( ListaDistPastilhas, [], [ (Dist, ( ObjX, ObjY ) ) | R ] ),
	get_dir( pastilhas, (PacX, PacY), (ObjX, ObjY), [[ (0, (PacX, PacY)) ]], Free, Pastilhas, Dec ).




%[(-8,6),(-5,7),(-5,2),(-7,9),(-8,-9),(-7,-3),(-4,-3),(-5,4),(-5,0),(-2,-7),(-4,7),(-7,7),(-4,9),(-3,-3),(-2,9),(-1,8),(-5,-7),(-5,-6),(-5,8),(-3,9),(-3,-6),(-5,-2),(-3,-9),(-5,-9),(-1,-3),(-2,-9),(-1,-4),(-5,-1),(-6,-7),(-2,5),(-8,9),(-8,-4),(-2,-3),(-5,6),(-1,9),(-5,9),(-5,-3),(-1,-8),(-8,5),(-4,-5),(-6,-3),(-5,-4),(-1,-5),(-3,-7),(-2,-5),(-7,5),(-1,7),(-8,-8),(-7,-9),(-3,7),(-8,7),(-6,7),(-6,-9),(-6,9),(-5,-5),(-8,-3),(-1,5),(-7,-7),(-2,7),(-1,-7),(-4,-9),(-3,6),(-1,-9),(-5,3),(-5,1),(-3,-5),(-5,5),(-6,5),(-8,-7),(-3,5),(-7,-6),(-7,-5)]