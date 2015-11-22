%%-- EM ALERTA %	 
pacman11(_,_,_,(Id,PacX,PacY,PacDir,M),_,Enemies,_,_,Free,_,_,Pastilhas,MaxPastilhas,Dec) :-
	alert_11((Id,PacX,PacY,PacDir,_), Enemies, 2,ListWithEnemy),
	ListWithEnemy \== [],
	first_11(ListWithEnemy, Ene),
	sucs_11_2((PacX,PacY), 0, Free, Sucs),
	setof((Valor,X,Y), X^Y^(member((_,X,Y), Sucs), casa_Value_11( Id,M,(X,Y),Ene, Pastilhas, MaxPastilhas, Valor)), Lista ),
	reverse(Lista, ListaR),
	first_11(ListaR, (_,Xx,Yy) ),
	viz_11(Dec,(PacX,PacY), (Xx,Yy) ).

%%-- A MIMICAR %
%para par
pacman11(N,_,_,(Id,_,_,_,_),_,[(IdAdv,_,_,Dir,_),_],_,_,_,_,_,_,_,Decisao) :-
	N < 10,
	MyIdMod is mod(Id, 2), MyIdMod == 0, AdvIdMod is mod(IdAdv, 2), AdvIdMod == 0,
	simetrico_11(Dir, Decisao).
pacman11(N,_,_,(Id,_,_,_,_),_,[_,(IdAdv,_,_,Dir,_)],_,_,_,_,_,_,_,Decisao) :-
	N < 10,
	MyIdMod is mod(Id, 2), MyIdMod == 0, AdvIdMod is mod(IdAdv, 2), AdvIdMod == 0,
	simetrico_11(Dir, Decisao).

%para impar
pacman11(N,_,_,(Id,_,_,_,_),_,[(IdAdv,_,_,Dir,_),_],_,_,_,_,_,_,_,Decisao) :-
	N < 10,
	MyIdMod is mod(Id, 2), MyIdMod == 1, AdvIdMod is mod(IdAdv, 2), AdvIdMod == 1,
	simetrico_11(Dir, Decisao).
pacman11(N,_,_,(Id,_,_,_,_),_,[_,(IdAdv,_,_,Dir,_)],_,_,_,_,_,_,_,Decisao) :-
	N < 10,
	MyIdMod is mod(Id, 2), MyIdMod == 1, AdvIdMod is mod(IdAdv, 2), AdvIdMod == 1,
	simetrico_11(Dir, Decisao).


%%-- A EXPLORAR %
pacman11(_,_,_,(Id,PacX,PacY,_,_),_,_,_,_,Free,_,_,Pastilhas,MaxPastilhas,Dec):-
	append(Pastilhas, MaxPastilhas, AllPastilhas),
	sortByY_11( AllPastilhas, [], PastilhasOrdenadas ),
	splitListByParity_11( Id, PastilhasOrdenadas, PacList ),
	calcDistAll_11( (PacX, PacY), PacList, Distancias ),
	setof( (D, (X, Y)), ( X^Y^member( (D, (X, Y)), Distancias ) ), [ (_, (X1, Y1)) | _] ),
	aStar11( (PacX,PacY), [[1, 0, (PacX, PacY)]], (X1, Y1), Free, [], [], Dec ).


%%-- simetrico_11 DE DIRECAO %
simetrico_11(0,0).
simetrico_11(180,180).
simetrico_11(90,270).
simetrico_11(270,90).


%%-- ALERTA %
alert_11(MyPos, EnemyPosList, AlertDist, ListWithEnemy):-
	insideRadius_11(MyPos,EnemyPosList, AlertDist,EnemyListClose),
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
	first_11(List, (_,PosX,PosY,_,_)),
	X is MyX - PosX,
	Y is MyY - PosY,
	X2 is X^2,
	Y2 is Y^2,
	Sq is X2+ Y2,
	Res is sqrt(Sq),
	AlertDistX is AlertDist + 1,
	Res =< AlertDistX,
	tail_11(List,R),
	insideRadius_11((_,MyX,MyY,_,_),R, AlertDist, EnemyListCloseTemp),
	first_11(List,H),
	EnemyListClose = [H|EnemyListCloseTemp].
	
insideRadius_11(Pos,List, AlertDist, EnemyListClose):-
	tail_11(List,R),
	insideRadius_11(Pos,R, AlertDist, EnemyListClose2),
	append([],EnemyListClose2, EnemyListClose).

%'verifica se os inimigos estão com medo inferior a alertDist.Retorna a lista de inimigos com medo inferior a alertDist ou vazio.'
haveLowFear_11([],_,[]).
haveLowFear_11(EnemyList,AlertDist, Res):-
	first_11(EnemyList, (_,PosX,PosY,_,M)),
	M  < AlertDist,
	tail_11(EnemyList,T),
	haveLowFear_11( T,AlertDist, ResTemp),
	Res = [(PosX,PosY) | ResTemp].
	
haveLowFear_11(EnemyList,AlertDist,Res):-
	tail_11(EnemyList,T),
	haveLowFear_11(T,AlertDist,ResTemp),
	append([],ResTemp,Res).

avaliaPosicao_11(Id,Medo,(PosX,PosY), Enemy, Pastilhas, MaxPastilhas, Valor ):-
	(enemyField_11((Id,PosX,PosY)); Medo > 1),
	manhatan_11((PosX,PosY), Enemy, Dist),
	comPastilha_11((PosX,PosY), Pastilhas, V),
	comMaxPastilha_11((PosX,PosY), MaxPastilhas, V2),
	Raiz is Dist * Dist,	
	Raiz1 is Raiz + 1,
	Temp is -10/Raiz1,
	Temp2 is Temp + V,
	Valor is Temp2 + V2.
	
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
		V is 10.
comMaxPastilha_11(_,_, 0).

first_11([],[]).	
first_11([ H|_], H).
tail_11([],[]).
tail_11([_|T], T).

%%-- ALGORITMO DE PROCURA DE CAMINHO OPTIMO %
%%-- A* BASE  %
aStar11( Me, [ [ _, _, (X, Y) | R ] | _ ], Goal, _, _, _, Sol ):-
	manhatan_11( (X, Y), Goal, 0 ),
	reverse( [(X, Y) | R], [ _, (PinX, PinY) | _ ]),
	viz_11( Sol, Me, (PinX, PinY) ).

%%-- GERA SUCS %
aStar11( Me, [ [ _, C, (X, Y) | R ] | Tree ], Goal, Free, Visitados, Expandidos, Sol ):-
	sucs_11( (X, Y), Free, Visitados, SucsList ),
	SucsCusto is C + 1,
	applyHeuristic_11( SucsCusto , SucsList, Goal, SucsHeuristic ),
	filter_11( SucsHeuristic, Expandidos, SucsFiltered),
	actualiza_expandidos_11( SucsFiltered, Expandidos, NewExpandidos ),
	append_all_11( SucsFiltered, [ (X, Y) | R ], NewSubTree ),
	append( NewSubTree, Tree, TTT ),
	setof( [ Hs, Cs, (Xs, Ys) | Rs ], Cs^Xs^Ys^member( [ Hs, Cs, (Xs, Ys) | Rs ], TTT ),  NNN ),
	append( [(X, Y)], Visitados, NewVisitados ),
	aStar11( Me, NNN, Goal, Free, NewVisitados, NewExpandidos, Sol ).

%%-- CASO ALTERNATIVO %
aStar11( Me, _, _, Free, _, _, Dec ):-
	findall( D, (viz_11( D, Me, (X, Y) ), member( (X, Y), Free )), L ),
	random_member(Dec,L).

%%-- ACTUALIZA EXPANDIDOS %
actualiza_expandidos_11( [], Exp, Exp).
actualiza_expandidos_11( [ [ H, _, (X, Y) ]|T ], Exp, Sol ):-
	\+member( (_, (X, Y)), Exp ),
	append( [(H, (X, Y))], Exp, NewExp ),
	actualiza_expandidos_11( T, NewExp, Sol ).

%%-- SUBSTITUI EXPANDIDOS %
actualiza_expandidos_11( [ [ H, _, (X, Y) ]|T ], Exp, Sol ):-
	member( (_, (X, Y)), Exp ),
	substitui_expandidos_11( (H, (X, Y)), Exp, NewExp ),
	actualiza_expandidos_11( T, NewExp, Sol ).

substitui_expandidos_11( (H, (X, Y)), [ (He, (Xe, Ye)) | R ], Sol ):-
	(X \= Xe ; Y \= Ye),
	substitui_expandidos_11( (H, (X, Y)), R, Rec ),
	Sol = [ (He, (Xe, Ye)) | Rec ].
substitui_expandidos_11( (H, (X, Y)), [ (_, (Xe, Ye)) | R ], Sol ):-
	X == Xe, Y == Ye,
	Sol = [ (H, (X, Y)) | R ].

%%-- APPEND ALL %
append_all_11( [], _, [] ).
append_all_11( [ [H, C, (X,Y)] | R ], L, Sol ):-
	append( [H, C, (X,Y)], L, This ),
	append_all_11( R, L, Rec ),
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

calcDistAll_11( _, [], [] ).
calcDistAll_11( Objectivo, [ Cur | R ], Res ):-
	manhatan_11( Cur, Objectivo, D ),
	M = (D, Cur),
	calcDistAll_11( Objectivo, R, Res2 ),
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
sortByY_11( [], Acc, Acc ).
sortByY_11( [ (X, Y) | R ], Acc, Sol ):-
	sortByY_11Aux( (X, Y), Acc, NewAcc ),
	sortByY_11(R, NewAcc, Sol).

sortByY_11Aux( (X, Y), [], [(X, Y)] ).
sortByY_11Aux( (X, Y), [ (Ax, Ay) | R ], Sol ):-
	Y > Ay,
	sortByY_11Aux( (X, Y), R, Rec ),
	Sol = [ (Ax, Ay) | Rec ].
sortByY_11Aux( (X, Y), [ (Ax, Ay) | R ], Sol ):-
	( (Y < Ay) ; ( Y == Ay )),
	Sol = [ (X, Y), (Ax, Ay) | R ].	


%%-- DIVIDE PASTILHAS POR PARIDADE DE AGENTE %
splitListByParity_11( Value, L, R ):-
	0 =:= mod(Value, 2),
	length(L, Len),
	Lim is Len / 2,
	ceiling(Lim, Limit),
	split_until_11(Limit, L, R ).

splitListByParity_11( Value, L, R ):-
	1 =:= mod(Value, 2),
	reverse( L, RevList ),
	length(L, Len),
	Lim is Len / 2,
	floor(Lim, Limit),
	split_until_11(Limit, RevList, R ).

split_until_11( 0, _, [] ).
split_until_11( _, [], [] ).
split_until_11( Limit, [P|R], List ):-
	NewLimit is Limit - 1,
	split_until_11( NewLimit, R, Rec ),
	List = [ P | Rec ].

