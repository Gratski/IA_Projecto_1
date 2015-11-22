%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Comportamento  Pacman 33
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%pacman_teste(
  %%Clock
  %15,

  %%ClockLimit
  %300,

  %%Score
  %0,

  %%Me
  %(1,-3,1,180,0),

  %%Partner
  %(0,-8,1,90,0),

  %%OtherTeam
  %[(2,3,-3,270,0),(3,7,1,270,0)],

  %%HomeBase 
  %(-9,1),

  %%HisBase
  %(8,1),

  %%FreeCells
  %[(4,-4),(-5,-4),(5,9),(-6,5),(-6,7),(3,-5),(7,9),(-2,-3),(7,-7),(-1,7),(-8,5),(-5,-9),(7,1),(-2,5),(-3,3),(8,-1),(4,7),(-1,8),(8,3),(-1,4),(-4,-5),(7,8),(4,8),(3,-9),(-3,-2),(-3,-6),(-6,-3),(0,9),(-2,-1),(7,6),(-2,3),(7,-1),(1,3),(0,-8),(2,-1),(-1,-8),(-2,-7),(7,5),(-1,-5),(6,-6),(-4,-9),(-8,-1),(1,-1),(6,-3),(-7,9),(-5,7),(-8,-4),(0,3),(2,7),(-1,-3),(1,9),(-1,-9),(1,-9),(-7,-6),(-6,-7),(-1,-1),(2,6),(3,7),(-8,-5),(-3,-1),(-5,-1),(-9,-1),(5,-7),(6,-7),(-7,-5),(-7,-1),(-3,9),(-3,7),(0,-4),(0,1),(2,9),(0,8),(-4,7),(-9,3),(-4,-3),(-7,3),(0,7),(2,-6),(-6,-9),(3,9),(-7,-7),(-7,7),(5,-9),(1,-7),(6,7),(0,-3),(-3,-3),(-6,9),(-8,8),(5,5),(7,-5),(-8,-7),(-3,0),(2,-2),(-3,-9),(4,0),(4,6),(1,-3),(-5,-3),(7,-8),(-9,1),(-5,-6),(0,4),(-5,5),(1,-5),(-3,1),(-2,-9),(-4,1),(-5,-2),(-8,-9),(2,1),(-7,-3),(-8,1),(-7,5),(4,1),(-3,5),(6,9),(4,-9),(-5,9),(-8,-3),(4,4),(1,5),(7,-4),(3,-3),(-2,-5),(-5,3),(-5,-5),(7,3),(-2,9),(6,-9),(4,-3),(5,1),(2,-3),(-7,1),(-8,3),(-5,6),(0,-7),(-2,7),(6,-1),(-5,1),(4,-6),(-1,-7),(2,3),(-5,8),(-1,-4),(-3,6),(-8,-8),(3,1),(4,9),(-1,9),(2,0),(7,-3),(2,5),(-3,-7),(-5,-7),(-6,1),(2,-9),(4,-7),(-5,0),(4,-2),(4,3),(7,-9),(-1,3),(4,2),(-1,1),(5,-3),(4,-5),(6,-5),(4,5),(-1,5),(6,3),(0,-1),(-8,7),(-8,6),(6,1),(-3,-5),(0,5),(-7,-9),(2,2),(8,1),(-4,9),(7,7),(5,7),(-8,9),(-3,2),(4,-1),(0,-5),(6,5),(2,-5),(2,-7),(-5,4),(-5,2),(0,-9),(1,7)],

  %%MyP
  %[(-7,9),(-1,9),(-5,-1),(-5,2),(-3,-9),(-1,-7),(-3,-5),(-5,3),(-6,-7),(-1,-3),(-8,6),(-8,7),(-3,7),(-3,-3),(-1,-9),(-7,5),(-4,-5),(-7,-7),(-8,5),(-5,-9),(-3,-7),(-2,-9),(-2,-7),(-2,7),(-5,8),(-7,-3),(-7,-5),(-1,-4),(-5,-4),(-8,-9),(-3,-6),(-6,5),(-1,7),(-5,1),(-6,7),(-8,-8),(-5,-2),(-5,-7),(-3,9),(-1,8),(-5,0),(-5,-3),(-8,-7),(-8,-4),(-8,-3),(-7,-9),(-2,-3),(-7,7),(-5,7),(-5,5),(-2,5),(-1,-8),(-3,6),(-8,9),(-3,5),(-7,-6),(-1,5),(-4,9),(-5,9),(-4,-3),(-4,7),(-2,-5),(-2,9),(-4,-9),(-5,4),(-6,-3),(-1,-5),(-5,-5),(-5,-6),(-5,6),(-6,9),(-6,-9)],
  
  %%MYSupP
  %[(-8,-5),(-8,8)],

  %%HisP
  %[(5,-9),(6,-5),(7,-7),(4,1),(4,-7),(7,-9),(2,7),(7,7),(2,-5),(1,-3),(6,7),(4,0),(5,5),(0,-4),(2,-3),(4,-1),(4,-3),(4,-5),(2,-9),(0,9),(3,9),(2,-7),(1,-7),(4,6),(7,9),(0,-7),(0,-5),(0,5),(3,-9),(6,-3),(4,-9),(3,-3),(4,2),(1,-5),(4,-2),(6,5),(7,6),(4,-4),(6,-7),(2,6),(2,-6),(1,5),(4,-6),(1,-9),(0,-3),(5,-3),(4,4),(3,-5),(0,8),(4,7),(1,7),(6,9),(6,-9),(2,9),(4,9),(4,8),(7,-4),(6,-6),(2,5),(4,5),(1,9),(5,9),(4,3),(0,-8),(7,5),(5,7),(7,-3),(5,-7),(0,-9),(3,7),(7,-8),(0,7)],

  %%HisSupP
  %[(7,-5),(7,8)],
  %Decisao)

% pacman(Clock, ClockLimit, Score, Me, Partner, OtherTeam, HomeBase , HisBase, FreeCells, MyP, MYSupP, HisP, HisSupP, Decisao)



%% ATAQUE
%%%%%%%%%%%% AGENTE 1
%% pacman33(_,_,_,(Id,X,Y,Dir,_),_,_,HomeBase,HisBase,FreeCells,_,_,_,[H],Dec):-
%%   if_red(HomeBase,1,3,AgenteEu),
%%   Id == AgenteEu,
%%   aStar((X,Y),H,FreeCells,Dec).

% Ataca o que está asserted, se estiver na lista de super pastilhas
pacman33(_,_,_,(Id,X,Y,Dir,_),_,_,HomeBase,HisBase,FreeCells,_,_,_,HisSupP,Dec):-
  if_red(HomeBase,1,3,AgenteEu),
  Id == AgenteEu,
  ataque_target(Target),
  member(Target,HisSupP),
  aStar((X,Y),Target,FreeCells,Dec).

% Faz assert de uma superpastilha e ataca-a
pacman33(_,_,_,(Id,X,Y,Dir,_),_,_,HomeBase,HisBase,FreeCells,_,_,_,[H|R],Dec):-
  if_red(HomeBase,1,3,AgenteEu),
  Id == AgenteEu,
  assert(ataque_target(H)),
  aStar((X,Y),H,FreeCells,Dec).

% Pastilha a 1 passo
pacman33(_,_,_,(Id,X,Y,Dir,_),_,_,HomeBase,HisBase,FreeCells,_,_,HisP,_,Dec):-
  if_red(HomeBase,1,3,AgenteEu),
  Id == AgenteEu,
  findall(D,(viz(D,(X,Y),Viz),member(Viz,FreeCells),member(Viz,HisP)),AlvosPossiveis),
  random_member(Dec,AlvosPossiveis).

% Pastilha a 2 passos
pacman33(_,_,_,(Id,X,Y,Dir,_),_,_,HomeBase,HisBase,FreeCells,_,_,HisP,_,Dec):-
  if_red(HomeBase,1,3,AgenteEu),
  Id == AgenteEu,
  findall(D,(viz(D,(X,Y),Viz),member(Viz,FreeCells),viz(_,Viz,VizViz),member(VizViz,FreeCells),member(VizViz,HisP)),AlvosPossiveis),
  random_member(Dec,AlvosPossiveis).

% Pastilha a 3 passos
pacman33(_,_,_,(Id,X,Y,Dir,_),_,_,HomeBase,HisBase,FreeCells,_,_,HisP,_,Dec):-
  if_red(HomeBase,1,3,AgenteEu),
  Id == AgenteEu,
  findall(D,(viz(D,(X,Y),Viz),member(Viz,FreeCells),viz(_,Viz,VizViz),member(VizViz,FreeCells),viz(D,VizViz,VizVizViz),member(VizVizViz,FreeCells),member(VizVizViz,HisP)),AlvosPossiveis),
  random_member(Dec,AlvosPossiveis).

% Random dir
pacman33(_,_,_,(Id,X,Y,Dir,_),_,_,HomeBase,HisBase,FreeCells,_,_,HisP,_,Dec):-
  if_red(HomeBase,1,3,AgenteEu),
  Id == AgenteEu,
  random_member(Dec,[0,90,180,270]).

:- dynamic defesa_id/1.
:- dynamic ataque_target/1.


%% DEFESA
%%%%%%%%%%%% AGENTE 2
pacman33(_,_,_,(Id,X,Y,Dir,_),_,[(Id2,X2,Y2,_,_),(Id3,X3,Y3,_,_)],HomeBase,HisBase,FreeCells,_,_,_,_,Dec):-
  if_red(HomeBase,0,2,AgenteEu),
  Id == AgenteEu,
  \+no_meu_territorio((X2,Y2),HomeBase),
  \+no_meu_territorio((X3,Y3),HomeBase),
  casas_no_meio(Y2,Y3,FreeCells,HomeBase,Casas),
  random_member(Casa,Casas),
  aStar((X,Y),Casa,FreeCells,Dec).

pacman33(_,_,_,(Id,X,Y,Dir,_),_,[(Id2,X2,Y2,_,_),(Id3,X3,Y3,_,_)],HomeBase,HisBase,FreeCells,_,_,_,_,Dec):-
  if_red(HomeBase,0,2,AgenteEu),
  Id == AgenteEu,
  no_meu_territorio((X2,Y2),HomeBase),
  no_meu_territorio((X3,Y3),HomeBase),
  get_id_mais_proximo((X,Y),[(Id2,X2,Y2,_,_),(Id3,X3,Y3,_,_)],Id2),
  get_casa_from_id(Id2,[(Id2,X2,Y2,_,_),(Id3,X3,Y3,_,_)],Casa),
  aStar((X,Y),Casa,FreeCells,Dec).

pacman33(_,_,_,(Id,X,Y,Dir,_),_,OtherTeam,HomeBase,HisBase,FreeCells,_,_,_,_,Dec):-
  if_red(HomeBase,0,2,AgenteEu),
  Id == AgenteEu,
  get_id_mais_proximo((X,Y),OtherTeam,Id2),
  get_casa_from_id(Id2,OtherTeam,Casa),
  no_meu_territorio(Casa,HomeBase),
  aStar((X,Y),Casa,FreeCells,Dec).

pacman33(_,_,_,(Id,X,Y,Dir,_),_,OtherTeam,HomeBase,HisBase,FreeCells,_,_,_,_,Dec):-
  if_red(HomeBase,0,2,AgenteEu),
  Id == AgenteEu,
  get_id_mais_longe((X,Y),OtherTeam,Id2),
  get_casa_from_id(Id2,OtherTeam,Casa),
  no_meu_territorio(Casa,HomeBase),
  aStar((X,Y),Casa,FreeCells,Dec).





get_id_mais_proximo((X,Y),[(Id2,X2,Y2,_,_),(Id3,X3,Y3,_,_)],Id):-
  dist_mann((X,Y),(X2,Y2),Dist2),
  dist_mann((X,Y),(X3,Y3),Dist3),
  Dist2 < Dist3,
  Id = Id2.

get_id_mais_proximo((X,Y),[(Id2,X2,Y2,_,_),(Id3,X3,Y3,_,_)],Id):-
  Id = Id3.


get_id_mais_longe((X,Y),[(Id2,X2,Y2,_,_),(Id3,X3,Y3,_,_)],Id):-
  dist_mann((X,Y),(X2,Y2),Dist2),
  dist_mann((X,Y),(X3,Y3),Dist3),
  Dist2 >= Dist3,
  Id = Id2.

get_id_mais_longe((X,Y),[(Id2,X2,Y2,_,_),(Id3,X3,Y3,_,_)],Id):-
  Id = Id3.


get_casa_from_id(Id,[(IdTarget,XTarget,YTarget,_)|_],(X,Y)):-
  Id =:= IdTarget,
  X = XTarget,
  Y = YTarget.

get_casa_from_id(Id,[_|(IdTarget,XTarget,YTarget,_)],(X,Y)):-
  Id =:= IdTarget,
  X = XTarget,
  Y = YTarget.


casas_no_meio(Y,Y2,Free,HomeBase,Casas):-
  YTarget is ceiling((abs(Y - Y2) / 2)),
  findall((X,YTarget),(member((X,YTarget),Free),no_meu_territorio((X,YTarget),HomeBase)),Casas).

% Devolve o valor correcto conforme a cor da equipa
% primeiro argumento tem de ser tuplo com coord. da nossa base
if_red((X,_),Red,Blue,Result):-
  X < 0,
  Result = Red.
  
if_red((X,_),Red,Blue,Result):-
  X >= 0,
  Result = Blue.

% Distancia de Mannhatan entre dois pares de coordenadas
dist_mann((X1,Y1),(X2,Y2),Result):-
  NX is X1 - X2,
  NY is Y1 - Y2,
  Result is abs(NX) + abs(NY).

% Distancia Euclidiana entre dois pares de coordenadas
dist_eucl((X1,Y1),(X2,Y2),Result):-
  NX is X1 - X2,
  NY is Y1 - Y2,
  Result is sqrt(abs(NX)**2 + abs(NY)**2).

% Calcula se as coordenadas pertencem à mesma casa
% Pode ser usado para ver o pacman chegou ao seu target
mesma_casa((X1,Y1),(X2,Y2)):-
  X1 =:= X2,
  Y1 =:= Y2.

% Sucede se a casa dada está no nosso territorio
% segundo argumento tem de ser tuplo com coord. da nossa base
no_meu_territorio((X,_),(X2,_)):-
  X2 < 0,
  X < 0.

no_meu_territorio((X,_),(X2,_)):-
  X2 > 0,
  X >= 0.

% Sucede se a casa dada tem alguma pastilha ou super pastilha
casa_tem_pastilha(Casa,MyP,MySupP):-
  member(Casa,MyP);member(Casa,MySupP).


% EM TESTES
% aStar((1,1),(3,2),[(1,1),(1,0),(0,1),(2,1),(1,2),(1,3),(2,2),(3,1),(2,0),(3,2)], Dec).

% (CasaActual, CasaTarget, ListaDeCasasFree, Dec)
aStar((X,Y),(XTarget,YTarget),Free,Dec):-
  findall((ValorFinal-1-[Viz]),(viz(Dir,(X,Y),Viz),member(Viz,Free),dist_eucl(Viz,(XTarget,YTarget),ValorEstimado),ValorFinal is ValorEstimado + 1),LPorVisitar),
  keysort(LPorVisitar,LPorVisitar2),
  %% print_vou_visitar((X,Y),LPorVisitar2,[(X,Y)]),
  aStar((X,Y),(XTarget,YTarget),Free,LPorVisitar2,[(X,Y)],Dec).


% (CasaActual, CasaTarget, ListaDeCasasFree, ListaCasasPorVisitar, ListaCasasVisitadas, Dec)
aStar(CasaActual,CasaObjectivo,Free,[CEst-CReal-[(X,Y)|R]|_],LVisitados,Dec):-
  mesma_casa((X,Y),CasaObjectivo),
  %% print_vou_visitar((X,Y)),
  %% write('CHEGOU AO FIM!'),nl,
  %% print_lista([(X,Y)|R]),
  last([(X,Y)|R],Ultimo),
  viz(Dec,CasaActual,Ultimo).

aStar(CasaActual,(XTarget,YTarget),Free,[CEst-CReal-[(X,Y)|RCaminho]|R],LVisitados,Dec):-
  %% print_vou_visitar((X,Y)),
  append(LVisitados,[(X,Y)],LVisitados2),
  findall((ValorFinal-CustoRealTotal-[Viz,(X,Y)|RCaminho]),(viz(Dir,(X,Y),Viz),member(Viz,Free),\+member(Viz,LVisitados2),dist_eucl(Viz,(XTarget,YTarget),ValorEstimado),CustoRealTotal is CReal + 1,ValorFinal is CustoRealTotal + ValorEstimado,para_visitar_aux(Viz,ValorFinal,R)),LPorVisitar),
  elimina_pior(LPorVisitar,R,R2),
  append(LPorVisitar,R2,LPorVisitar2),
  keysort(LPorVisitar2,LPorVisitar3),
  %% print_vou_visitar((X,Y),LPorVisitar3,LVisitados2),
  aStar(CasaActual,(XTarget,YTarget),Free,LPorVisitar3,LVisitados2,Dec).


% [CEst-CReal-(X,Y)|R]
para_visitar_aux((X,Y),MelhorValor,[]):-
  true.

para_visitar_aux((X,Y),MelhorValor,[CEst-CReal-[(X2,Y2)|_]|R]):-
  X =:= X2, Y =:= Y2,
  MelhorValor > CEst,!,true.

para_visitar_aux((X,Y),MelhorValor,[CEst-CReal-[(X2,Y2)|_]|R]):-
  X =:= X2, Y =:= Y2,
  MelhorValor =< CEst,!,fail.

para_visitar_aux((X,Y),MelhorValor,[CEst-CReal-[(X2,Y2)|_]|R]):-
  %% write('tira um'),nl,
  para_visitar_aux((X,Y),MelhorValor,R).

% Elimina um valor se este tiver menor Valor de heuristica
elimina_pior(LPorVisitar,R,Result):-
  member(Valor-_-[(X,Y)|_],LPorVisitar),member(Valor2-_-[(X2,Y2)|_],R),
  X == X2, Y == Y2,
  Valor > Valor2,
  select(Valor2-_-[(X2,Y2)|_],R,Result).

elimina_pior(LPorVisitar,R,Result):-
  Result = R.

print_vou_visitar((X,Y)):-
  write('Vou visitar ('),write(X),write(','),write(Y),write(')'),nl.

print_vou_visitar((X,Y),LPorVisitar,LVisitados):-
  write('Visitei ('),write(X),write(','),write(Y),write(')'),nl,
  write('Lista de Visitados:'),nl,
  print_lista(LVisitados),
  write('Lista para Visitar:'),nl,
  print_lista(LPorVisitar),nl.


get_first([First|_],Result):-
  Result = First.

suc(Score, ListaPastilhas, ListaSupPastilhas, Direcao, Target, ListaInimigos, Result):-
  NY is L + 1.


% Possivel calculo do tabuleiro (1 estado)
% Cada estado tem em conta:
% Score, lista de pastilhas, lista de supPastilhas, nova posicao do pacman, posicao do target?, lista de inimigos
% score(-(distancia do pacman ao target)+(sum das distancias aos inimigos))*if_red(homebase,1,-1,result)

% +50-2+20
% -50+2-20

evaluate_state(Score,Result):-
  Result = Score.

print_lista([H]):-
  write(H),nl,write('Terminou'),nl.

print_lista([H|R]):-
  write(H),nl,print_lista(R).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% JA VINHA DO PROF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% senao tenta ir para uma orientacao ao acaso, preferencialmente
% que nao seja pela inversa da actual, procurando novo corredor
%% pacman33(_,_,_,(_,X,Y,Dir,_),_,_,_,_,Free,_,_,_,_,Dec) :-
%%   findall(D,(viz(D,(X,Y),Viz),member(Viz,Free)),L),
%%   dirAcasoPrefNaoVoltarAtras(L,Dir,Dec).



% se orientacao inversa for a unica
dirAcasoPrefNaoVoltarAtras([Dec],_,Dec).

% senao escolha uma ao acaso diferente da inversa da actual
% O predicao builtin select/3 selecciona um elemento de uma
% lista, devolvendo tambem a lista sem o elemento seleccionado.
dirAcasoPrefNaoVoltarAtras(Possiveis,D,Dec) :-
  inverte(D,Inv),
  select(Inv,Possiveis,L),
  random_member(Dec,L).

% casa vizinha a norte
viz(0,(X,Y),(X,NY)) :-
  NY is Y + 1.

% casa vizinha a sul
viz(180,(X,Y),(X,NY)) :-
  NY is Y - 1.

% casa vizinha a leste
viz(90,(X,Y),(NX,Y)) :-
  NX is X + 1.

% casa vizinha a oeste
viz(270,(X,Y),(NX,Y)) :-
  NX is X - 1.



% as orientacoes inversas

%  a inversa de leste eh oeste 
inverte(90,270).

% a inversa de norte eh sul
inverte(0,180).

% a inversa de sul eh norte
inverte(180,0).

% a inversa de oeste eh leste
inverte(270,90).