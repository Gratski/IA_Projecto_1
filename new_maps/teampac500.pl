%%-- EM ALERTA %	 
pacman500(_,_,_,(Id,PacX,PacY,PacDir,M),_,Enemies,_,_,Free,_,_,Pastilhas,MaxPastilhas,Dec) :-
	write('primeira Func '),
	alert_11((Id,PacX,PacY,PacDir,_), Enemies, 2,ListWithEnemy),
	ListWithEnemy \== [],
	write('Alerta ativado '),write(ListWithEnemy), nl,
	first(ListWithEnemy, Ene),
	sucs_11_2((PacX,PacY), 0, Free, Sucs),
	setof((Valor,X,Y), X^Y^(member((_,X,Y), Sucs), casa_Value_11( Id,M,(X,Y),Ene, Pastilhas, MaxPastilhas, Valor)), Lista ),
	write('Lista (valor Suc)'), write(Lista), nl,
	reverse(Lista, ListaR),
	first(ListaR, (_,Xx,Yy) ),
	write('Viz:'), write(PacX), write(PacY), write(','), write(Xx), write(Yy), nl,
	viz_11(Dec,(PacX,PacY), (Xx,Yy) ).

%%-- A MIMICAR %
%para par
pacman500(N,_,_,(Id,_,_,_,_),_,[(IdAdv,_,_,Dir,_),_],_,_,_,_,_,_,_,Decisao) :-
	N < 10,
	MyIdMod is mod(Id, 2), MyIdMod == 0, AdvIdMod is mod(IdAdv, 2), AdvIdMod == 0,
	simetrico(Dir, Decisao).
pacman500(N,_,_,(Id,_,_,_,_),_,[_,(IdAdv,_,_,Dir,_)],_,_,_,_,_,_,_,Decisao) :-
	N < 10,
	MyIdMod is mod(Id, 2), MyIdMod == 0, AdvIdMod is mod(IdAdv, 2), AdvIdMod == 0,
	simetrico(Dir, Decisao).

%para impar
pacman500(N,_,_,(Id,_,_,_,_),_,[(IdAdv,_,_,Dir,_),_],_,_,_,_,_,_,_,Decisao) :-
	N < 10,
	MyIdMod is mod(Id, 2), MyIdMod == 1, AdvIdMod is mod(IdAdv, 2), AdvIdMod == 1,
	simetrico(Dir, Decisao).
pacman500(N,_,_,(Id,_,_,_,_),_,[_,(IdAdv,_,_,Dir,_)],_,_,_,_,_,_,_,Decisao) :-
	N < 10,
	MyIdMod is mod(Id, 2), MyIdMod == 1, AdvIdMod is mod(IdAdv, 2), AdvIdMod == 1,
	simetrico(Dir, Decisao).


%%-- A EXPLORAR %
pacman500(_,_,_,(Id,PacX,PacY,_,_),_,_,_,_,Free,_,_,Pastilhas,MaxPastilhas,Dec):-
	append(Pastilhas, MaxPastilhas, AllPastilhas),
	sortByY( AllPastilhas, [], PastilhasOrdenadas ),
	write('Pastilhas Ordenadas'), nl,
	write(PastilhasOrdenadas), nl,
	splitListByParity( Id, PastilhasOrdenadas, PacList ),
	calcDistAll( (PacX, PacY), PacList, Distancias ),
	setof( (D, (X, Y)), ( X^Y^member( (D, (X, Y)), Distancias ) ), [ (_, (X1, Y1)) | _] ),
	aStar11( (PacX,PacY), [[1, 0, (PacX, PacY)]], (X1, Y1), Free, [], [], Dec ).


%%-- SIMETRICO DE DIRECAO %
simetrico(0,0).
simetrico(180,180).
simetrico(90,270).
simetrico(270,90).


%%-- ALERTA %
alert_11(MyPos, EnemyPosList, AlertDist, ListWithEnemy):-
	write('alert1'), nl,
	insideRadius_11(MyPos,EnemyPosList, AlertDist,EnemyListClose),
	write('alert2 '),write(EnemyListClose), nl,
	haveLowFear_11(EnemyListClose,AlertDist, ListWithEnemy),!.

%'verifica se estamos no campo do inimigo. Return True ou False.'
enemyField_11((Id,PosX,_)):-
	Id < 2,
	PosX > (-1).

enemyField_11((Id,PosX,_)):-
	Id > 2,
	PosX < 0.

%'verifica se algum inimigo esta a uma distancia inferior a alertDist.'
insideRadius_11(_,[], _, []).
insideRadius_11((_,MyX,MyY,_,_),List, AlertDist, EnemyListClose):-
	first(List, (_,PosX,PosY,_,_)),
	X is MyX - PosX,
	Y is MyY - PosY,
	X2 is X^2,
	Y2 is Y^2,
	Sq is X2+ Y2,
	Res is sqrt(Sq),
	AlertDistX is AlertDist + 1,
	Res =< AlertDistX,
	tail(List,R),
	insideRadius_11((_,MyX,MyY,_,_),R, AlertDist, EnemyListCloseTemp),
	first(List,H),
	EnemyListClose = [H|EnemyListCloseTemp].
	
insideRadius_11(Pos,List, AlertDist, EnemyListClose):-
	tail(List,R),
	insideRadius_11(Pos,R, AlertDist, EnemyListClose2),
	append([],EnemyListClose2, EnemyListClose).

%'verifica se os inimigos estão com medo inferior a alertDist.Retorna a lista de inimigos com medo inferior a alertDist ou vazio.'
haveLowFear_11([],_,[]).
haveLowFear_11(EnemyList,AlertDist, Res):-
	first(EnemyList, (_,PosX,PosY,_,M)),
	M  < AlertDist,
	tail(EnemyList,T),
	haveLowFear_11( T,AlertDist, ResTemp),
	Res = [(PosX,PosY) | ResTemp].
	
haveLowFear_11(EnemyList,AlertDist,Res):-
	tail(EnemyList,T),
	haveLowFear_11(T,AlertDist,ResTemp),
	append([],ResTemp,Res).

avaliaPosicao_11(Id,Medo,(PosX,PosY), Enemy, Pastilhas, MaxPastilhas, Valor ):-
	(enemyField_11((Id,PosX,PosY)); Medo > 1),
	% 'ALterar Para Dist Real'
	manhatan_11((PosX,PosY), Enemy, Dist),
	write('distE= '),write(Dist),nl,
	comPastilha_11((PosX,PosY), Pastilhas, V),
	comMaxPastilha_11((PosX,PosY), MaxPastilhas, V2),
	Raiz is Dist * Dist,	
	Raiz1 is Raiz + 1,
	Temp is -10/Raiz1,
	write('Temp '), write(Temp), nl,
	write('V '), write(V), nl,
	write('V2 '), write(V2), nl,
	Temp2 is Temp + V,
	write('Temp2 '), write(Temp2), nl,
	Valor is Temp2 + V2,
	write('Valor: '), write(Valor), nl.
	
%'caso seja no nosso Terreno'
avaliaPosicao_11(_, _, (PosX,PosY), Enemy, _, _, Valor ):-
	manhatan_11((PosX,PosY), Enemy, Dist),
	write(Dist),nl,
	Raiz is Dist * Dist,
	Raiz2 is Raiz +1,
	Valor is 10/Raiz2.
	
casa_Value_11(Id,M,(PosX,PosY), Enemy, Pastilhas, MaxPastilhas, V):-
	avaliaPosicao_11(Id,M,(PosX,PosY), Enemy, Pastilhas, MaxPastilhas, Valor ),!,
	V is Valor.
	
	
%'se a casa tiver pastilha devolve valor 5'
comPastilha_11(Pos, Pastilhas, V ):-
		member(Pos, Pastilhas),
		V is 5.
comPastilha_11(_,_, 0).

%'se a casa tiver pastilha devolve valor 5'
comMaxPastilha_11(Pos, Pastilhas, V ):-
		member(Pos, Pastilhas),
		V is 10,
		write('Pastilhas'), write(Pastilhas), nl, 
		write('Membro'), nl.
comMaxPastilha_11(_,_, 0).

first([],[]).	
first([ H|_], H).
second([_,S|_], S).
tail([],[]).
tail([_|T], T).
soma(X,Y, Sum):-
	Sum is X + Y.



%%-- CALCULA DISTANCIA A PASTILHAS DE VARIOS PONTOS %

%%-- FUNCAO ALERTA %
alerta( _, [], [] ).
alerta( Me, [ (_,Ex,Ey,_,_) | R ], Res ):-
	manhatan_11( Me, (Ex, Ey), D ),
	write('EU: '), write(Me), nl,
	write('Enemy: '), write((Ex, Ey)), nl, 
	D < 3,
	alerta( Me, R, Rec ),
	Res = [ (Ex, Ey) | Rec ].
alerta( Me, [ (_,Ex,Ey,_,_) | R ], Res ):-
	write('EU: '), write(Me), nl,
	write('Enemy: '), write((Ex, Ey)), nl, 
	manhatan_11( Me, (Ex, Ey), D ),
	(D > 3 ; D == 3),
	alerta( Me, R, Res ).

%%-- A* BASE  %
%aStar11((-9,1), [ [7, 0, (-9,1)] ] , (7,-9), [(-4,9),(-2,9),(0,-8),(8,1),(-8,1),(-6,5),(-8,-7),(0,5),(4,-9),(0,-5),(-5,-6),(2,-7),(-7,7),(0,-1),(-9,1),(-2,-3),(-8,6),(-1,1),(4,-4),(6,-5),(-3,-2),(0,-9),(-5,-9),(4,9),(4,-6),(2,-2),(7,9),(5,1),(2,6),(-5,-3),(8,-1),(2,-6),(-9,-1),(-3,9),(4,-1),(7,3),(-8,-1),(7,-9),(-7,-5),(-8,8),(1,-5),(4,-7),(-2,-5),(-4,-9),(5,-7),(-7,5),(6,-1),(2,-1),(5,-3),(-4,7),(-3,-5),(4,3),(0,-4),(-7,-6),(4,-5),(-6,-9),(-6,1),(-1,-1),(-4,1),(1,-3),(2,9),(0,7),(6,1),(-8,9),(4,1),(7,5),(-7,-1),(-3,5),(-3,1),(1,7),(-2,7),(1,3),(7,-3),(6,7),(-1,4),(-7,1),(0,-3),(-9,3),(-4,-5),(-5,-7),(2,-9),(4,8),(-5,6),(-2,3),(-8,5),(-7,3),(-8,-5),(1,-9),(4,5),(-5,8),(-1,-5),(-5,-4),(5,5),(-5,5),(0,9),(-1,-3),(7,-5),(-2,-9),(4,6),(-1,-9),(4,-3),(3,-9),(3,1),(-6,-7),(8,3),(5,-9),(-8,3),(5,7),(6,-7),(-5,-2),(7,7),(-1,-7),(-3,-9),(-8,-8),(-1,9),(7,-4),(2,0),(-5,-1),(-5,3),(7,6),(3,-3),(-3,-7),(6,-6),(-8,-9),(4,4),(7,1),(-4,-3),(2,-3),(-6,7),(-5,1),(-5,7),(-3,-3),(-3,-1),(1,9),(4,7),(-3,-6),(2,5),(2,7),(1,5),(7,-8),(2,-5),(7,-7),(0,4),(-7,-3),(0,3),(-6,-3),(-5,4),(-5,-5),(6,-3),(-3,0),(-1,-8),(4,-2),(-2,-1),(-1,7),(1,-1),(-8,-4),(-8,7),(0,-7),(-3,3),(-2,-7),(3,-5),(-3,6),(-7,-9),(-2,5),(2,3),(-1,3),(0,1),(2,1),(1,-7),(-5,2),(6,3),(-1,5),(3,9),(-7,9),(-3,2),(-6,9),(6,5),(2,2),(-3,7),(7,-1),(4,0),(-1,-4),(-8,-3),(-5,0),(-7,-7),(6,-9),(5,9),(3,7),(-1,8),(4,2),(0,8),(6,9),(-5,9),(7,8)], [], [], Sol).

aStar11( Me, [ [ _, C, (X, Y) | R ] | _ ], Goal, _, _, _, Sol ):-
	manhatan_11( (X, Y), Goal, 0 ),
	reverse( [(X, Y) | R], [ EE, (PinX, PinY) | REV ]),
	write('Custo'), nl,
	write(C), nl,
	write('CAMINHO: '),nl,
	write([ EE, (PinX, PinY) | REV ]), nl,
	viz_11( Sol, Me, (PinX, PinY) ).

aStar11( Me, [ [ _, C, (X, Y) | R ] | Tree ], Goal, Free, Visitados, Expandidos, Sol ):-

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

	aStar11( Me, NNN, Goal, Free, NewVisitados, NewExpandidos, Sol ).

aStar11( _, _, _, _, _, _, 90 ).

%%-- ACTUALIZA EXPANDIDOS %
actualiza_expandidos( [], Exp, Exp).
actualiza_expandidos( [ [ H, _, (X, Y) ]|T ], Exp, Sol ):-
	\+member( (_, (X, Y)), Exp ),
	append( [(H, (X, Y))], Exp, NewExp ),
	actualiza_expandidos( T, NewExp, Sol ).

%%-- SUBSTITUI EXPANDIDOS %
actualiza_expandidos( [ [ H, _, (X, Y) ]|T ], Exp, Sol ):-
	member( (_, (X, Y)), Exp ),
	substitui_expandidos( (H, (X, Y)), Exp, NewExp ),
	actualiza_expandidos( T, NewExp, Sol ).

substitui_expandidos( (H, (X, Y)), [ (He, (Xe, Ye)) | R ], Sol ):-
	(X \= Xe ; Y \= Ye),
	substitui_expandidos( (H, (X, Y)), R, Rec ),
	Sol = [ (He, (Xe, Ye)) | Rec ].
substitui_expandidos( (H, (X, Y)), [ (_, (Xe, Ye)) | R ], Sol ):-
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

filter_11( [ [H, _, (X, Y)] | R], Expandidos, Sol ):-
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

sucs_11_2((PosX,PosY), C, Free, Sucs):-
	findall((Nc,(X,Y)), (viz_11(_,(PosX,PosY),(X,Y)),member((X,Y),Free),Nc is C + 1), Sucs).

%%-- MANHANTAN %
manhatan_11( (X1, Y1), (X2, Y2), Dist ):-
	DistX is X1 - X2, abs(DistX, AbsX),
	DistY is Y1 - Y2, abs(DistY, AbsY),
	Dist is AbsX + AbsY.

calcDistAll( _, [], [] ).
calcDistAll( Objectivo, [ Cur | R ], Res ):-
	manhatan_11( Cur, Objectivo, D ),
	M = (D, Cur),
	calcDistAll( Objectivo, R, Res2 ),
	Res = [M | Res2].

%%-- VIZINHO %
viz_11(0,(X,Y),(X,NY)) :-
	NY is Y + 1.
viz_11(180,(X,Y),(X,NY)) :-
	NY is Y - 1.
viz_11(90,(X,Y),(NX,Y)) :-
	NX is X + 1.
viz_11(270,(X,Y),(NX,Y)) :-
	NX is X - 1.

%%-- ORDENA PARES POR YY %
sortByY( [], Acc, Acc ).
sortByY( [ (X, Y) | R ], Acc, Sol ):-
	sortByYAux( (X, Y), Acc, NewAcc ),
	sortByY(R, NewAcc, Sol).

sortByYAux( (X, Y), [], [(X, Y)] ).
sortByYAux( (X, Y), [ (Ax, Ay) | R ], Sol ):-
	Y > Ay,
	sortByYAux( (X, Y), R, Rec ),
	Sol = [ (Ax, Ay) | Rec ].
sortByYAux( (X, Y), [ (Ax, Ay) | R ], Sol ):-
	( (Y < Ay) ; ( Y == Ay )),
	Sol = [ (X, Y), (Ax, Ay) | R ].	


%%-- DIVIDE PASTILHAS POR PARIDADE DE AGENTE %
splitListByParity( Value, L, R ):-
	0 =:= mod(Value, 2),
	length(L, Len),
	Lim is Len / 2,
	ceiling(Lim, Limit),
	split_until(Limit, L, R ).

splitListByParity( Value, L, R ):-
	1 =:= mod(Value, 2),
	reverse( L, RevList ),
	length(L, Len),
	Lim is Len / 2,
	floor(Lim, Limit),
	split_until(Limit, RevList, R ).

split_until( 0, _, [] ).
split_until( _, [], [] ).
split_until( Limit, [P|R], List ):-
	NewLimit is Limit - 1,
	split_until( NewLimit, R, Rec ),
	List = [ P | Rec ].

