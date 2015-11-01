%sucessores
sucs( Cur, Free, Sucs ):-
	findall( Suc , ( viz( _, Cur, Suc ), member( Suc, Free ) ), Sucs).

%calcular distancia de manhatan de n posicoes até um destino
calcDistAll( Objectivo, [], Acc, Acc ).
calcDistAll( Objectivo, [ Cur | R ], Acc, Res ):-
	manhatan( Cur, Objectivo, M ),
	calcDistAll( Objectivo, R, [ M | Acc ], Res ).

%criar sub arvores
appendAll( [], _, Acc, Acc ).
appendAll( [ Cur | R ], Tree, Acc, Res ):-
	append(Cur, Tree, SubTree),
	appendAll( R, Tree, [ SubTree | Acc ], Res ).

%obtem direcoes
%caso base
get_dir( pastilhas, (PacX, PacY), ( ObjX, ObjY ), [ [ (D, (X, Y)) | IR ] | OR ], Free, Gums, Dec ):-
	member( (X, Y), Gums ),
	reverse( [ (D, (X, Y)) | IR ], [ (_ ,( HeadX, HeadY )) | R ] ),
	viz(Dec, (PacX, PacY), (X, Y)).

%explorer :P
get_dir( pastilhas, (PacX, PacY), ( ObjX, ObjY ), [ [ (D, (X, Y)) | IR ] | OR ], Free, Gums, Dec ):-
	sucs( (X, Y), Free, Sucs ),
	calcDistAll( Sucs, ( ObjX, ObjY ), [], L ), 
	sort_by_custo( L, [], SortedL ),
	appendAll( SortedL, [ (D, (X, Y)) | IR ], [], SubTree ),
	get_dir( pastilhas, (PacX, PacY), ( ObjX, ObjY ), [ SubTree | OR ], Free, Gums, Dec ).

	