:- use_module(library(lists)).
%pacman(Clock, ClockLimit, Score, Me, Partner, OtherTeam, HomeBase , HisBase, FreeCells, MyP, MYSupP, HisP, HisSupP, Decisao) :-



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%_NOTAS
% por agora estou apenas a testar a busca com o pacman 0
% ainda nao temos heuristica
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Metodos oficiais

% pacman com ID 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pacman0(_,_,_,(1,X,Y,_,_),_,_,_,_,Free,_,_,_,_,Dec) :-
	findall(D,(viz(D,(X,Y),Viz),member(Viz,Free)),L),
	random_member(Dec,L).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Metodos auxiliares

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
	append([Cur], Tree, SubTree),
	appendAll( R, Tree, [ SubTree | Acc ], Res ).


%obtem proxima direcao
%caso base
%get_dir( pastilhas, (PacX, PacY), _, [ [ (D, (X, Y)) | IR ] | _ ], _, Gums, Dec ):-
	member( (X, Y), Gums ),
	%reverse( [ _, (D, (X, Y)) | IR ], [ (_ ,( HeadX, HeadY )) | _ ] ),
	%viz(Dec, (PacX, PacY), (HeadX, HeadY)).
	Dec is 180.
%explorer :P
get_dir( pastilhas, (PacX, PacY), ( ObjX, ObjY ), [ [ (D, (X, Y)) | IR ] | OR ], Free, Gums, Dec ):-
	sucs( (X, Y), Free, Sucs ),
	calcDistAll( ( ObjX, ObjY ), Sucs, [], L ), 
	sort_by_custo( L, [], SortedL ),
	appendAll( SortedL, [ (D, (X, Y)) | IR ], [], SubTree ), reverse(SubTree, SortedSubTree),
	get_dir( pastilhas, (PacX, PacY), ( ObjX, ObjY ), [ SortedSubTree | OR ], Free, Gums, Dec ).




% vizinhos
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
viz(0,(X,Y),(X,NY)) :-
	NY is Y + 1.
viz(180,(X,Y),(X,NY)) :-
	NY is Y - 1.
viz(90,(X,Y),(NX,Y)) :-
	NX is X + 1.
viz(270,(X,Y),(NX,Y)) :-
	NX is X - 1.



% pacman com ID 0
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pacman0(_,_,_,(0,PacX,PacY,Dir,_),_,_,_,_,Free,_,_,Pastilhas,_,Dec):-
	calcDistAll( (PacX, PacY), Pastilhas, [], ListaDistPastilhas ),
	sort_by_custo( ListaDistPastilhas, [], [ (Dist, ( ObjX, ObjY ) ) | R ] ),
	get_dir( pastilhas, (PacX, PacY), (ObjX, ObjY), [[ (0, (PacX, PacY)) ]], Free, Pastilhas, Dec ).


%[(2,3),(-2,-1),(1,7),(-2,-3),(4,8),(-4,7),(-1,4),(-5,9),(4,1),(6,9),(-1,-1),(-3,9),(-1,-3),(7,-4),(-3,-1),(1,5),(7,-5),(1,3),(4,3),(7,7),(6,-6),(-9,1),(6,-7),(4,-4),(-1,-9),(5,7),(0,4),(-7,-1),(0,7),(-5,6),(6,7),(0,-7),(7,1),(1,-5),(-3,-2),(3,9),(-6,1),(1,-9),(-4,1),(7,8),(2,6),(-2,3),(0,5),(-8,-4),(4,6),(-2,-9),(3,-5),(-2,5),(2,2),(2,-7),(0,-3),(0,8),(-5,2),(4,5),(-5,7),(-3,-5),(6,-1),(7,-8),(-7,-7),(4,2),(-8,-7),(3,1),(-7,-6),(6,-9),(-6,7),(7,-9),(-3,6),(4,-6),(-5,-1),(2,-6),(-3,0),(-2,-7),(5,9),(-8,-3),(-4,9),(2,9),(-7,-5),(0,9),(-8,-1),(-5,8),(-1,-5),(-1,-8),(-7,3),(-5,1),(-8,-8),(0,3),(2,-2),(4,-7),(7,5),(-3,-7),(6,-3),(1,9),(-6,-7),(5,-7),(7,6),(-4,-5),(-8,1),(-4,-3),(4,4),(7,-3),(4,9),(-8,-9),(-5,0),(2,5),(0,-9),(4,-1),(-1,9),(7,3),(-1,3),(-2,7),(-1,7),(4,-9),(5,5),(-5,4),(-3,2),(8,-1),(-5,-7),(-3,-3),(-1,8),(2,-3),(5,1),(2,-1),(-1,-7),(-5,-2),(-5,3),(-2,9),(6,-5),(0,-5),(-8,3),(-5,-6),(-6,5),(2,1),(6,1),(-3,1),(-4,-9),(-8,-5),(-2,-5),(0,-8),(6,3),(3,7),(4,-5),(-3,-6),(0,-4),(8,3),(7,9),(-7,7),(4,-2),(7,-1),(8,1),(-7,-3),(-8,7),(3,-3),(-6,-3),(-8,9),(-3,3),(-9,-1),(-7,5),(-9,3),(-5,-3),(0,-1),(6,5),(-3,5),(-6,-9),(3,-9),(-1,-4),(2,7),(1,-1),(5,-3),(-1,1),(2,-5),(-7,9),(-7,1),(-3,-9),(-5,-9),(2,-9),(-6,9),(-8,5),(4,0),(5,-9),(-8,6),(-5,-5),(4,-3),(7,-7),(1,-3),(0,1),(-5,5),(-5,-4),(2,0),(4,7),(-7,-9),(1,-7),(-8,8),(-3,7),(-1,5)]