:- use_module(library(lists)).

%'Verifica se existe algum inimigo a uma distancia inferior a AlertDist.Return a list with enemies or empty.'
%'MyPos = (_,_,_,_,_),como vem nos argumentos do netlogo.'
alerta(MyPos, EnemyPosList, AlertDist, ListWithEnemy):-
	enemyField(MyPos),
	insideRadius(MyPos,EnemyPosList, AlertDist,EnemyListClose),
	haveLowFear(EnemyListClose,AlertDist, ListWithEnemy).

	
%'verifica se estamos no campo do inimigo. Return True ou False.'
enemyField((Id,PosX,_,_,_)):-
	Id < 2,
	PosX > -1 .
	
enemyField((Id,PosX,_,_,_)):-
	Id > 1,
	PosX < 0.

%'verifica se algum inimigo esta a uma distancia inferior a alertDist.'
insideRadius(_,[], _, []).
insideRadius((_,MyX,MyY,_,_),List, AlertDist, EnemyListClose):-
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
	insideRadius((_,MyX,MyY,_,_),R, AlertDist, EnemyListCloseTemp),
	first(List,H),
	EnemyListClose = [H|EnemyListCloseTemp].
	
insideRadius(Pos,List, AlertDist, EnemyListClose):-
	tail(List,R),
	insideRadius(Pos,R, AlertDist, EnemyListClose2),
	append([],EnemyListClose2, EnemyListClose).

%'verifica se os inimigos estão com medo inferior a alertDist.Retorna a lista de inimigos com medo inferior a alertDist ou vazio.'
haveLowFear([],_,[]).
haveLowFear(EnemyList,AlertDist, Res):-
	write(EnemyList),nl,
	first(EnemyList, (_,PosX,PosY,_,M)),
	write('Medo ='), write(M), nl,
	M  < AlertDist,
	tail(EnemyList,T),
	write(T), nl,
	haveLowFear( T,AlertDist, ResTemp),
	Res = [(PosX,PosY) | ResTemp].
	
haveLowFear(EnemyList,AlertDist,Res):-
	write('Fail Lista='), write(EnemyList),nl,
	tail(EnemyList,T),
	haveLowFear(T,AlertDist,ResTemp),
	append([],ResTemp,Res).

aStar(Inicial,Final, Solution);-
	
%'funçoes basicas sobre listas.'
first([],[]).	
first([ H|_], H).
tail([],[]).
tail([_|T], T).
