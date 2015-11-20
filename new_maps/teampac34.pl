pacman34(1,_,_,(_,PacX,PacY,PacDir,_),_,_,_,_,Free,_,_,Pastilhas,MaxPastilhas,Decisao):-
	Decisao is 0.

%para par
pacman34(N,_,_,(Id,PacX,PacY,PacDir,_),_,[(IdAdv,_,_,Dir,_),_],_,_,Free,_,_,Pastilhas,MaxPastilhas,Decisao) :-
	N < 15,
	MyIdMod is mod(Id, 2), MyIdMod == 0, AdvIdMod is mod(IdAdv, 2), AdvIdMod == 0,
	simetrico_11(Dir, Decisao).
pacman34(N,_,_,(Id,PacX,PacY,PacDir,_),_,[_,(IdAdv,_,_,Dir,_)],_,_,Free,_,_,Pastilhas,MaxPastilhas,Decisao) :-
	N < 15,
	MyIdMod is mod(Id, 2), MyIdMod == 0, AdvIdMod is mod(IdAdv, 2), AdvIdMod == 0,
	simetrico_11(Dir, Decisao).
%para impar
pacman34(N,_,_,(Id,PacX,PacY,PacDir,_),_,[(IdAdv,_,_,Dir,_),_],_,_,Free,_,_,Pastilhas,MaxPastilhas,Decisao) :-
	N < 15,
	MyIdMod is mod(Id, 2), MyIdMod == 1, AdvIdMod is mod(IdAdv, 2), AdvIdMod == 1,
	simetrico_11(Dir, Decisao).
pacman34(N,_,_,(Id,PacX,PacY,PacDir,_),_,[_,(IdAdv,_,_,Dir,_)],_,_,Free,_,_,Pastilhas,MaxPastilhas,Decisao) :-
	N < 15,
	MyIdMod is mod(Id, 2), MyIdMod == 1, AdvIdMod is mod(IdAdv, 2), AdvIdMod == 1,
	simetrico_11(Dir, Decisao).



pacman34(_,_,_,(_,PacX,PacY,PacDir,_),_,Enemies,_,_,Free,_,_,Pastilhas,MaxPastilhas,Dec) :-
	concat_11( Pastilhas, MaxPastilhas, TodasPastilhas ),
	calcDistAll_11((PacX,PacY),TodasPastilhas,ListDistPastilhas),
	setof((X,Y,Z),X^Y^member((X,Y,Z), ListDistPastilhas), ListaOrdenada),
	first(ListaOrdenada,(_,Px,Py)),
	aStar_11((0,(PacX,PacY)),(Px,Py), Free, Solution),
	second(Solution, (_,Xx,Yy)),
	viz_11(Dec,(PacX,PacY),(Xx,Yy)).

pacman34(_,_,_,(Id,PacX,PacY,PacDir,_),_,Enemies,_,_,Free,_,_,Pastilhas,MaxPastilhas,Dec) :-
	alert_11((_,PacX,PacY,PacDir,_), Enemies, 2,ListWithEnemy)
	sucs_11((PacX,PacY), 0, Free, Sucs),

%'Dá um valor a uma certa posição consoante estejamos no nosso ou territorio deles e se nos afastamos ou aproximamos do adversãorio.'
%'Caso Posição seja no terreno adversario'
avaliaPosicao_11(Id,(PosX,PosY), Enemy, Pastilhas, MaxPastilhas Valor ):-
	enemyField_11((Id,PosX,PosY));
	distReal_11((PosX,PosY), Enemy, Dist), // Nao implementado
	comPastilha_11((PosX,PosY), Pastilhas V),
	comMaxPastilha_11(PosX,PosY, MaxPastilha V2),
	Valor is Dist+V+V2.
	
%'caso seja no nosso Terreno'
avaliaPosicao_11(Id,(PosX,PosY), Enemy, Pastilhas, MaxPastilhas Valor ):-
	
		

	



%'se a casa tiver pastilha devolve valor 5'
comPastilha_11(Pos, Pastilhas, V ):-
		member(Pos, Pastilhas),
		V is 5.
comPastilha_11(_,_, 0).

%'se a casa tiver pastilha devolve valor 5'
comMaxPastilha_11(Pos, Pastilhas, V ):-
		member(Pos, Pastilhas),
		V is 5.
comMaxPastilha_11(_,_, 0).


%'Verifica se existe algum inimigo a uma distancia inferior a AlertDist.Return a list with enemies or empty.'
%'MyPos = (_,_,_,_,_),como vem nos argumentos do netlogo.'
alerta_11(MyPos, EnemyPosList, AlertDist, ListWithEnemy):-
	enemyField_11(MyPos),
	insideRadius_11(MyPos,EnemyPosList, AlertDist,EnemyListClose),
	haveLowFear_11(EnemyListClose,AlertDist, ListWithEnemy),
	\+ ListWithEnemy == [].	
	
%'verifica se estamos no campo do inimigo. Return True ou False.'
enemyField_11((Id,PosX,PosY):-
	Id < 2,
	PosX > -1 .
	
enemyField_11((Id,PosX,PosY)):-
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

	
calcDistAll_11( _, [], [] ).
calcDistAll_11( Objectivo, [ Cur | R ], Res ):-
	manhatan_11( Cur, Objectivo, D ),
	M = (D, Cur),
	calcDistAll_11( Objectivo, R, Res2 ),
	Res = [M | Res2].
	
manhatan_11( (X1, Y1), (X2, Y2), Dist ):-
	DistX is X1 - X2, abs(DistX, AbsX),
	DistY is Y1 - Y2, abs(DistY, AbsY),
	Dist is AbsX + AbsY.
	
first([],[]).	
first([ H|_], H).
second([F,S|_], S).
tail([],[]).
tail([_|T], T).
soma(X,Y, Sum):-
	Sum is X + Y.

%'Vizinhos'
%'norte'
viz_11(0,(X,Y),(X,NY)) :-
	NY is Y + 1.
%'Sul'
viz_11(180,(X,Y),(X,NY)) :-
	NY is Y - 1.
%'este'
viz_11(90,(X,Y),(NX,Y)) :-
	NX is X + 1.
%'oeste'
viz_11(270,(X,Y),(NX,Y)) :-
	NX is X - 1.

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
		
%'Devolve a lista de sucessores de uma determinada posição e tem em conta o custo(neste caso soma 1).'
%'A lista devolvida tem o formato: [((X,Y),C)|R].'
sucs_11((PosX,PosY), C, Free, Sucs):-
	findall((Nc,(X,Y)), (viz_11(_,(PosX,PosY),(X,Y)),member((X,Y),Free),Nc is C + 1), Sucs).

concat_11(L1, L2, Res):-
	append(L1, L2, Res).

simetrico_11(0,0).
simetrico_11(180,180).
simetrico_11(90,270).
simetrico_11(270,90).

%pacman34(15,300,0,(1,0,-3,90,0),(0,0,5,0,0),[(3,7,1,270,0),(2,-2,5,270,0)],(-9,1),(8,1),[(-6,-9),(7,-8),(-9,1),(-3,7),(6,-9),(7,-7),(2,9),(4,-7),(-4,-5),(-8,8),(2,-2),(7,8),(4,-9),(-1,8),(8,-1),(-5,3),(0,-7),(-1,-1),(-4,-3),(6,1),(8,1),(7,1),(-5,2),(2,3),(-1,3),(7,-4),(-8,6),(3,9),(-5,8),(0,-3),(1,5),(-7,1),(4,-2),(-1,4),(4,-6),(1,-5),(5,5),(7,7),(2,-1),(5,-3),(6,-6),(4,2),(-3,1),(3,-9),(1,3),(-3,-6),(-5,0),(-1,-8),(0,5),(0,8),(-9,3),(7,-5),(5,-7),(-3,-9),(0,-9),(-7,-9),(3,1),(4,-4),(2,7),(-2,3),(1,-7),(-2,9),(7,-9),(2,-9),(4,4),(2,5),(4,5),(2,2),(1,-3),(-3,3),(5,9),(4,8),(6,7),(-1,9),(7,9),(-8,-1),(-3,9),(-5,4),(-5,1),(2,6),(-5,-3),(4,1),(-3,6),(1,9),(-6,7),(-3,2),(4,0),(6,3),(-6,9),(3,-3),(0,-8),(8,3),(-2,-5),(-5,-1),(1,-9),(6,-3),(-8,9),(0,-4),(6,-1),(-1,-3),(5,-9),(-5,9),(-5,7),(-8,-8),(-2,-1),(6,9),(-7,-1),(4,-3),(3,7),(-6,-3),(4,6),(-8,3),(-5,-9),(0,-1),(-1,1),(0,9),(-3,-3),(2,-7),(-6,-7),(6,-7),(-8,-9),(-6,5),(0,-5),(6,5),(-5,-6),(-5,-7),(-2,-7),(-8,-3),(7,6),(-7,-6),(-2,-3),(7,3),(-7,9),(-7,-7),(-9,-1),(-1,7),(-4,9),(-5,6),(-6,1),(4,-5),(-8,1),(-3,-7),(-5,-2),(5,1),(7,-3),(7,-1),(-3,-2),(-3,5),(-4,-9),(2,-5),(7,5),(4,3),(0,3),(-8,-5),(0,4),(-7,5),(4,-1),(-3,-5),(-8,-4),(-1,-7),(-3,-1),(6,-5),(-8,7),(-4,1),(2,0),(-1,-5),(1,-1),(3,-5),(-2,5),(4,9),(1,7),(2,1),(-4,7),(-7,-5),(-8,5),(-7,-3),(-5,5),(-1,5),(0,1),(-3,0),(-2,-9),(-1,-9),(-2,7),(-7,7),(-7,3),(2,-6),(-1,-4),(5,7),(-8,-7),(0,7),(2,-3),(-5,-5),(4,7),(-5,-4)],[(-8,5),(-5,9),(-4,9),(-3,6),(-5,8),(-3,-9),(-1,-7),(-5,-9),(-2,-9),(-6,7),(-4,-9),(-3,5),(-5,6),(-5,-3),(-3,-7),(-4,-3),(-6,-3),(-6,9),(-1,-3),(-7,-7),(-5,1),(-8,-3),(-3,-6),(-5,-2),(-8,-7),(-1,-8),(-3,-5),(-1,-9),(-5,5),(-8,6),(-8,9),(-8,-8),(-5,7),(-7,-5),(-8,-9),(-1,8),(-8,-4),(-6,-7),(-5,0),(-7,7),(-7,-3),(-8,7),(-6,-9),(-5,4),(-5,2),(-1,-5),(-7,5),(-5,-1),(-5,-6),(-7,9),(-4,7),(-5,-5),(-2,7),(-6,5),(-1,9),(-2,-3),(-1,7),(-4,-5),(-5,3),(-3,9),(-7,-9),(-2,-7),(-5,-4),(-5,-7),(-3,7),(-2,-5),(-7,-6),(-3,-3),(-2,9),(-1,-4)],[(-8,8),(-8,-5)],[(4,7),(4,-5),(4,6),(6,-9),(0,9),(6,-7),(4,-3),(3,-5),(5,-9),(7,-7),(5,-7),(2,7),(4,-9),(2,-5),(2,-6),(6,-6),(6,5),(4,-4),(6,-3),(1,-3),(3,-3),(7,7),(4,5),(4,-2),(4,3),(1,-5),(0,-9),(2,5),(4,8),(2,-9),(4,2),(3,9),(6,-5),(5,-3),(7,-3),(0,7),(7,9),(2,-3),(2,6),(7,6),(7,5),(5,7),(4,1),(6,7),(0,-4),(4,-7),(5,9),(1,-7),(3,7),(4,9),(7,-8),(4,-1),(4,4),(1,-9),(4,0),(0,-5),(6,9),(7,-4),(2,9),(5,5),(0,8),(7,-9),(3,-9),(1,7),(1,9),(0,-7),(2,-7),(4,-6),(0,-8),(1,5)],[(7,8),(7,-5)],Decisao).
%pacman34(23,300,0,(0,7,-3,90,0),(1,2,5,90,0),[(2,0,3,270,0),(3,-3,9,270,0)],(-9,1),(8,1),[(-1,-5),(5,-3),(-6,-9),(-3,3),(4,-5),(6,-6),(4,6),(6,7),(5,-7),(4,5),(-8,6),(4,-3),(-9,3),(4,-2),(-7,-6),(-6,-7),(-5,9),(-5,-2),(-7,9),(4,-6),(-1,-7),(1,3),(-8,3),(-3,7),(-1,7),(3,1),(7,3),(-1,-8),(6,-3),(2,-3),(4,-4),(-9,-1),(3,-5),(0,7),(1,9),(0,5),(5,9),(-5,-6),(4,0),(2,2),(-7,-5),(-1,3),(-3,-6),(7,6),(3,-3),(6,-5),(0,3),(2,-1),(6,-7),(0,8),(-5,1),(-4,-3),(-4,1),(-7,5),(-5,-3),(-7,3),(-2,3),(-5,3),(7,5),(8,1),(-1,-9),(6,3),(-9,1),(2,0),(5,-9),(-2,-5),(-6,5),(-5,2),(6,-9),(5,7),(1,-3),(-3,-1),(1,-1),(-7,7),(-6,7),(-8,1),(-1,-3),(-6,9),(-5,5),(-3,-3),(-8,-7),(7,-9),(-1,5),(4,7),(3,-9),(8,-1),(0,-3),(-8,5),(-7,-9),(1,7),(0,-1),(-8,-9),(-8,-5),(-4,-5),(-4,-9),(0,-4),(6,5),(-5,-4),(-8,-1),(4,8),(-8,7),(-3,5),(-6,-3),(-5,6),(-2,-1),(-5,-5),(2,-9),(5,1),(8,3),(0,-5),(0,1),(-7,-1),(-8,-3),(-1,4),(-8,-4),(-5,0),(-4,7),(4,-7),(1,5),(-5,4),(4,3),(-3,2),(-3,6),(-5,-1),(-4,9),(4,-1),(-1,8),(6,-1),(2,-2),(7,-7),(2,7),(2,6),(-8,-8),(-1,1),(0,9),(-3,9),(-2,9),(1,-9),(2,5),(3,7),(-8,8),(7,8),(-2,-3),(4,4),(-3,0),(7,-5),(0,-8),(4,2),(0,-9),(6,1),(-1,9),(-2,-9),(2,-7),(7,-1),(-7,-3),(-1,-1),(-2,5),(-1,-4),(0,4),(1,-7),(7,9),(2,9),(2,1),(-3,-5),(7,1),(-8,9),(7,-4),(2,-6),(-7,-7),(-7,1),(-3,-2),(7,-3),(4,9),(-5,7),(7,7),(-2,7),(-5,-9),(7,-8),(0,-7),(6,9),(-3,-9),(-5,-7),(3,9),(2,-5),(2,3),(1,-5),(4,1),(-2,-7),(-3,-7),(4,-9),(-6,1),(-3,1),(-5,8),(5,5)],[(-6,-3),(-7,7),(-5,-7),(-3,-9),(-8,-4),(-2,-3),(-5,-6),(-5,3),(-7,-3),(-7,9),(-5,-5),(-1,-3),(-2,-5),(-6,7),(-8,-8),(-1,-4),(-6,-7),(-8,-3),(-4,7),(-5,7),(-1,-8),(-1,-5),(-4,-5),(-7,5),(-5,2),(-8,-7),(-5,0),(-2,-9),(-5,5),(-4,-3),(-3,-3),(-7,-6),(-6,-9),(-8,6),(-5,-4),(-6,5),(-8,7),(-3,-6),(-8,-9),(-7,-9),(-1,-9),(-4,9),(-7,-5),(-1,-7),(-3,-5),(-5,-1),(-5,4),(-5,-3),(-8,9),(-8,5),(-7,-7),(-5,-2),(-5,6),(-4,-9),(-5,1),(-3,-7),(-5,-9),(-6,9),(-5,8),(-5,9),(-2,-7)],[(-8,-5),(-8,8)],[(2,-9),(2,-6),(2,6),(4,7),(0,8),(0,-4),(4,1),(7,5),(3,7),(4,0),(4,5),(1,7),(0,-8),(6,5),(7,-7),(0,-7),(4,-6),(5,5),(7,-8),(4,3),(6,-9),(4,-9),(3,-9),(7,9),(7,-4),(0,7),(2,-7),(7,6),(0,9),(6,-6),(4,8),(6,-5),(0,-5),(4,6),(1,-7),(6,7),(1,-9),(2,9),(4,-1),(5,-9),(5,9),(5,7),(4,9),(4,-4),(6,9),(4,4),(5,-7),(1,-5),(0,-9),(3,9),(7,7),(6,-7),(4,-2),(3,-5),(4,2),(4,-7),(2,-5),(2,7),(4,-5),(1,9),(7,-9)],[(7,8),(7,-5)],Decisao).
%pacman34(15,300,0,(0,-3,1,90,0),(1,-3,1,90,0),[(2,2,0,180,0),(3,2,0,180,0)],(-9,1),(8,1),[(6,-3),(7,1),(5,-7),(-8,-3),(-5,-6),(-3,-6),(-3,-1),(-3,5),(0,-3),(3,1),(6,-6),(2,0),(2,-2),(-6,-9),(4,8),(-7,-9),(2,-3),(4,-2),(-3,2),(-2,-7),(4,1),(0,7),(6,1),(-5,-5),(3,7),(8,1),(-1,8),(-2,-3),(4,4),(-5,-1),(1,-3),(-8,8),(4,7),(7,7),(5,9),(-5,8),(-7,7),(-5,-3),(3,-9),(-6,5),(2,3),(-9,-1),(7,8),(7,5),(-4,9),(1,7),(1,-1),(-6,7),(5,-3),(-1,3),(-5,4),(0,-7),(1,9),(-8,-1),(4,6),(-5,2),(-5,5),(-2,9),(2,-5),(-7,1),(-8,-4),(7,3),(-3,-2),(-4,-3),(-3,-9),(2,1),(-4,1),(-7,-1),(-5,6),(-4,7),(-1,5),(-5,0),(-2,-5),(-1,-1),(-4,-5),(4,-5),(4,-3),(8,3),(2,9),(-3,6),(7,9),(6,9),(2,-1),(-7,5),(-1,-8),(2,-9),(-1,9),(-5,7),(7,6),(5,1),(-7,3),(4,-9),(-2,-1),(4,2),(-1,-5),(-3,3),(-3,9),(-8,3),(-8,9),(2,-6),(8,-1),(0,-4),(-7,-5),(3,9),(4,9),(-6,9),(0,-8),(-4,-9),(-5,3),(7,-9),(2,6),(-3,1),(1,-9),(5,-9),(2,-7),(-2,7),(4,3),(0,9),(-2,5),(6,7),(-7,-3),(-6,1),(3,-5),(-5,-2),(-9,3),(-5,1),(4,-1),(1,3),(0,3),(-1,-4),(3,-3),(-7,-7),(4,0),(-3,0),(1,-7),(7,-3),(-3,7),(1,-5),(-5,-9),(-5,-7),(2,5),(-5,-4),(-1,4),(6,-5),(-3,-3),(-8,-5),(4,-7),(0,-5),(5,5),(-8,1),(-8,-8),(0,1),(-7,9),(-1,-7),(-9,1),(4,-4),(7,-5),(7,-8),(-1,-3),(-7,-6),(-8,7),(-1,7),(2,2),(-8,6),(6,3),(6,-9),(0,4),(-5,9),(-6,-3),(-2,-9),(0,-1),(-8,-9),(6,5),(-1,1),(0,-9),(7,-1),(6,-7),(-6,-7),(-8,-7),(1,5),(4,-6),(7,-7),(-2,3),(-3,-7),(5,7),(2,7),(-8,5),(6,-1),(0,5),(-3,-5),(-1,-9),(7,-4),(4,5),(0,8)],[(-7,-7),(-7,-3),(-6,7),(-1,-8),(-8,-4),(-2,9),(-2,5),(-3,-6),(-3,5),(-5,9),(-4,-9),(-3,7),(-6,-7),(-5,6),(-4,-5),(-5,4),(-7,9),(-8,-8),(-7,-5),(-5,2),(-1,8),(-6,9),(-5,5),(-1,-3),(-2,-5),(-3,-3),(-8,7),(-3,-7),(-3,9),(-5,-1),(-5,-4),(-2,-9),(-1,7),(-7,-6),(-8,-7),(-2,-3),(-6,-3),(-1,-4),(-2,-7),(-1,5),(-5,-2),(-5,7),(-6,-9),(-8,-3),(-8,5),(-2,7),(-4,9),(-1,-5),(-4,7),(-5,0),(-7,7),(-5,-6),(-1,-7),(-6,5),(-5,-7),(-7,5),(-5,8),(-4,-3),(-3,-9),(-1,-9),(-5,-3),(-8,9),(-5,-9),(-1,9),(-3,-5),(-5,1),(-7,-9),(-8,-9),(-3,6),(-5,3),(-5,-5),(-8,6)],[(-8,8),(-8,-5)],[(4,-9),(2,-6),(0,-7),(4,7),(6,-5),(5,-3),(1,-3),(7,-7),(2,6),(5,7),(4,-1),(7,-8),(7,5),(1,-7),(6,7),(4,-4),(2,9),(0,9),(7,7),(3,7),(0,-3),(6,5),(0,7),(4,0),(6,-7),(0,-4),(4,8),(7,-3),(3,-3),(4,-6),(6,-6),(3,-9),(4,-3),(5,5),(2,7),(2,-5),(0,8),(2,-9),(1,-9),(5,-9),(6,9),(4,5),(4,-7),(2,-7),(4,-5),(5,9),(2,5),(4,2),(2,-3),(0,-5),(4,3),(0,5),(3,-5),(4,-2),(1,5),(1,9),(4,1),(7,-4),(4,9),(7,6),(6,-9),(7,9),(3,9),(1,-5),(1,7),(4,6),(7,-9),(5,-7),(0,-8),(6,-3),(4,4),(0,-9)],[(7,8),(7,-5)],Decisao).