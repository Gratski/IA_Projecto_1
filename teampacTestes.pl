:- use_module(library(lists)).

%'Verifica se existe algum inimigo a uma distancia inferior a AlertDist.Return a list with enemies or empty.'
%'MyPos = (_,_,_,_,_),como vem nos argumentos do netlogo.'
alerta_11(MyPos, EnemyPosList, AlertDist, ListWithEnemy):-
	enemyField_11(MyPos),
	insideRadius_11(MyPos,EnemyPosList, AlertDist,EnemyListClose),
	haveLowFear_11(EnemyListClose,AlertDist, ListWithEnemy),	
	
%'verifica se estamos no campo do inimigo. Return True ou False.'
enemyField_11((Id,PosX,_,_,_)):-
	Id < 2,
	PosX > -1 .
	
enemyField_11((Id,PosX,_,_,_)):-
	Id > 1,
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
	insideRadius((_,MyX,MyY,_,_),R, AlertDist, EnemyListCloseTemp),
	first(List,H),
	EnemyListClose = [H|EnemyListCloseTemp].
	
insideRadius_11(Pos,List, AlertDist, EnemyListClose):-
	tail(List,R),
	insideRadius(Pos,R, AlertDist, EnemyListClose2),
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

%'Vizinhos'	
viz_11(0,(X,Y),(X,NY)) :-
	NY is Y + 1.
viz_11(180,(X,Y),(X,NY)) :-
	NY is Y - 1.
viz_11(90,(X,Y),(NX,Y)) :-
	NX is X + 1.
viz_11(270,(X,Y),(NX,Y)) :-
	NX is X - 1.
	
%'heuristica.'
manhatan_11( (X1, Y1), (X2, Y2), Dist ):-
	DistX is X1 - X2,
	abs(DistX, AbsX),
	DistY is Y1 - Y2, 
	abs(DistY, AbsY),
	Dist is AbsX + AbsY.


%'Devolve a lista de sucessores de uma determinada posição e tem em conta o custo(neste caso soma 1).'
%'A lista devolvida tem o formato: [((X,Y),C)|R].'
sucs_11((PosX,PosY), C, Free, Sucs):-
	findall((Nc,(X,Y)), (viz_11(_,(PosX,PosY),(X,Y)),member((X,Y),Free),Nc is C + 1), Sucs).
	
%'Procura o caminho mais curto entre 2 pontos. Devolve uma lista no formato [(Custo,Inicial),.....,(Custo,Final)].'	
aStar_11((C,Inicial),Final,Frees, Solution):-
	manhatan_11(Inicial, Final, Dist),
	Heur is Dist + C,
	aStarAux_11([(Heur ,[(C,Inicial)])], Final, [(C,Inicial)],Frees, 40,Solution).

%'aStarAux(ListaComCaminhos, EstadoFinal, ListaVisitados,ListaDeCasasLisvres, Solucao).'	
aStarAux_11( [(_,[(Cost,Final)|R]) |_], Final,_, Frees,_, Sol):-
	reverse([(Cost,Final)|R], Sol).

aStarAux_11( [(_,[(C,(PosX,PosY))|T])| Tail] , Final, Vs, Frees,0, Sol):-
	reverse([(C,(PosX,PosY))|T], Sol).

aStarAux_11( [(_,[(C,(PosX,PosY))|T])| Tail] , Final, Vs, Frees, Ni, Sol):-
		sucs_11((PosX,PosY),C, Frees,Sucs),
		write('Sucs='),write(Sucs), nl,
		setof( (Tot,[(C1,(X1,Y1)),(C,(PosX,PosY))|T]), C1^X1^X2^(member((C1,(X1,Y1)),Sucs), \+ member((_,(X1,Y1)),Vs) , manhatan_11((X1,Y1), Final, H), soma(H, C1, Tot)), List),
		reverse(List, NewList),
		write('NewList='),write(NewList), nl,
		findall((C1,(X1,Y1)), member((_,[(C1,(X1,Y1))| _] ), NewList), List2),
		write('List2='),write(List2),nl,
		append(List2, Vs, NovoVs),
		write('NovosVs='),write(NovoVs), nl,
		append(NewList, Tail, Todos),
		write('Todos='),write(Todos), nl,
		Nxxx is Ni - 1,
		aStarAux_11(Todos, Final, NovoVs, Frees, Nxxx, Sol).
			
%'funçoes basicas sobre listas.'
first([],[]).	
first([ H|_], H).
tail([],[]).
tail([_|T], T).
soma(X,Y, Sum):-
	Sum is X + Y.

%Frees:
%[(-4,9),(-2,9),(0,-8),(8,1),(-8,1),(-6,5),(-8,-7),(0,5),(4,-9),(0,-5),(-5,-6),(2,-7),(-7,7),(0,-1),(-9,1),(-2,-3),(-8,6),(-1,1),(4,-4),(6,-5),(-3,-2),(0,-9),(-5,-9),(4,9),(4,-6),(2,-2),(7,9),(5,1),(2,6),(-5,-3),(8,-1),(2,-6),(-9,-1),(-3,9),(4,-1),(7,3),(-8,-1),(7,-9),(-7,-5),(-8,8),(1,-5),(4,-7),(-2,-5),(-4,-9),(5,-7),(-7,5),(6,-1),(2,-1),(5,-3),(-4,7),(-3,-5),(4,3),(0,-4),(-7,-6),(4,-5),(-6,-9),(-6,1),(-1,-1),(-4,1),(1,-3),(2,9),(0,7),(6,1),(-8,9),(4,1),(7,5),(-7,-1),(-3,5),(-3,1),(1,7),(-2,7),(1,3),(7,-3),(6,7),(-1,4),(-7,1),(0,-3),(-9,3),(-4,-5),(-5,-7),(2,-9),(4,8),(-5,6),(-2,3),(-8,5),(-7,3),(-8,-5),(1,-9),(4,5),(-5,8),(-1,-5),(-5,-4),(5,5),(-5,5),(0,9),(-1,-3),(7,-5),(-2,-9),(4,6),(-1,-9),(4,-3),(3,-9),(3,1),(-6,-7),(8,3),(5,-9),(-8,3),(5,7),(6,-7),(-5,-2),(7,7),(-1,-7),(-3,-9),(-8,-8),(-1,9),(7,-4),(2,0),(-5,-1),(-5,3),(7,6),(3,-3),(-3,-7),(6,-6),(-8,-9),(4,4),(7,1),(-4,-3),(2,-3),(-6,7),(-5,1),(-5,7),(-3,-3),(-3,-1),(1,9),(4,7),(-3,-6),(2,5),(2,7),(1,5),(7,-8),(2,-5),(7,-7),(0,4),(-7,-3),(0,3),(-6,-3),(-5,4),(-5,-5),(6,-3),(-3,0),(-1,-8),(4,-2),(-2,-1),(-1,7),(1,-1),(-8,-4),(-8,7),(0,-7),(-3,3),(-2,-7),(3,-5),(-3,6),(-7,-9),(-2,5),(2,3),(-1,3),(0,1),(2,1),(1,-7),(-5,2),(6,3),(-1,5),(3,9),(-7,9),(-3,2),(-6,9),(6,5),(2,2),(-3,7),(7,-1),(4,0),(-1,-4),(-8,-3),(-5,0),(-7,-7),(6,-9),(5,9),(3,7),(-1,8),(4,2),(0,8),(6,9),(-5,9),(7,8)]
%'aStar'
%aStar_11( (0,(0,8)), (7,8), [(-4,9),(-2,9),(0,-8),(8,1),(-8,1),(-6,5),(-8,-7),(0,5),(4,-9),(0,-5),(-5,-6),(2,-7),(-7,7),(0,-1),(-9,1),(-2,-3),(-8,6),(-1,1),(4,-4),(6,-5),(-3,-2),(0,-9),(-5,-9),(4,9),(4,-6),(2,-2),(7,9),(5,1),(2,6),(-5,-3),(8,-1),(2,-6),(-9,-1),(-3,9),(4,-1),(7,3),(-8,-1),(7,-9),(-7,-5),(-8,8),(1,-5),(4,-7),(-2,-5),(-4,-9),(5,-7),(-7,5),(6,-1),(2,-1),(5,-3),(-4,7),(-3,-5),(4,3),(0,-4),(-7,-6),(4,-5),(-6,-9),(-6,1),(-1,-1),(-4,1),(1,-3),(2,9),(0,7),(6,1),(-8,9),(4,1),(7,5),(-7,-1),(-3,5),(-3,1),(1,7),(-2,7),(1,3),(7,-3),(6,7),(-1,4),(-7,1),(0,-3),(-9,3),(-4,-5),(-5,-7),(2,-9),(4,8),(-5,6),(-2,3),(-8,5),(-7,3),(-8,-5),(1,-9),(4,5),(-5,8),(-1,-5),(-5,-4),(5,5),(-5,5),(0,9),(-1,-3),(7,-5),(-2,-9),(4,6),(-1,-9),(4,-3),(3,-9),(3,1),(-6,-7),(8,3),(5,-9),(-8,3),(5,7),(6,-7),(-5,-2),(7,7),(-1,-7),(-3,-9),(-8,-8),(-1,9),(7,-4),(2,0),(-5,-1),(-5,3),(7,6),(3,-3),(-3,-7),(6,-6),(-8,-9),(4,4),(7,1),(-4,-3),(2,-3),(-6,7),(-5,1),(-5,7),(-3,-3),(-3,-1),(1,9),(4,7),(-3,-6),(2,5),(2,7),(1,5),(7,-8),(2,-5),(7,-7),(0,4),(-7,-3),(0,3),(-6,-3),(-5,4),(-5,-5),(6,-3),(-3,0),(-1,-8),(4,-2),(-2,-1),(-1,7),(1,-1),(-8,-4),(-8,7),(0,-7),(-3,3),(-2,-7),(3,-5),(-3,6),(-7,-9),(-2,5),(2,3),(-1,3),(0,1),(2,1),(1,-7),(-5,2),(6,3),(-1,5),(3,9),(-7,9),(-3,2),(-6,9),(6,5),(2,2),(-3,7),(7,-1),(4,0),(-1,-4),(-8,-3),(-5,0),(-7,-7),(6,-9),(5,9),(3,7),(-1,8),(4,2),(0,8),(6,9),(-5,9),(7,8)], Sol).
