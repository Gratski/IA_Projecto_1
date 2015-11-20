
%%%%%%%%%%%%
% Comportamento ZigZag Pacman
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%pacman(Clock, ClockLimit, Score, Me, Partner, OtherTeam, HomeBase , HisBase, FreeCells, MyP, MYSupP, HisP, HisSupP, Decisao) :-

% se pastilhas numa casa vizinha vou para la (qualquer delas)
pacman104(_,_,_,(Id,X,Y,_,M),_,Eles,_,_,_,_,_,Pasts,_,Dec) :-
	findall(D,(viz(D,(X,Y),Viz),member(Viz,Pasts),\+ naoSegura((X,Y),Id,M,Eles)),L),
	random_member(Dec,L).

% se pastilhas numa casa a 2 passos vou para la (qualquer delas)
pacman104(_,_,_,(Id,X,Y,_,M),_,Eles,_,_,Free,_,_,Pasts,_,Dec) :-
	findall(D,(viz(D,(X,Y),Viz),member(Viz,Free),viz(_,Viz,VizViz),member(VizViz,Pasts),\+ naoSegura((X,Y),Id,M,Eles)),L),
	random_member(Dec,L).

% se pastilhas numa casa a 3 passos vou para la (qualquer delas)
pacman104(_,_,_,(Id,X,Y,_,M),_,Eles,_,_,Free,_,_,Pasts,_,Dec) :-
	findall(D,(viz(D,(X,Y),Viz),member(Viz,Free),viz(_,Viz,VizViz),member(VizViz,Free), viz(_,VizViz,VizVizViz),member(VizVizViz,Pasts),\+ naoSegura((X,Y),Id,M,Eles)),L),
	random_member(Dec,L).

% se casa livre em frente e no territorio dele e pastilhas nem sinal a 1. 2 ou 3 passos
pacman104(_,_,_,(Id,X,Y,Dir,M),_,Eles,_,_,Free,_,_,_,_,Dir) :-
	random(X),X < 0.75,
	viz(Dir,(X,Y),Viz),
	member(Viz,Free),
	foraTerritorio(Viz,Id),
	\+ naoSegura((X,Y),Id,M,Eles).

% se parede em frente ou meu territorio e pastilhas nem ve-las
pacman104(_,_,_,(Id,X,Y,Dir,M),_,Eles,_,_,Free,_,_,_,_,Dec) :-
	findall(D,(viz(D,(X,Y),Viz),member(Viz,Free),foraTerritorio(Viz,Id),\+ naoSegura((X,Y),Id,M,Eles)),L),
	inversa(Dir,Inv),
	aoCalhaPrefiroNaoRegressar(L,Inv,Dec).


% se casa livre em frente e pastilhas nem sinal a 1. 2 ou 3 passos
pacman104(_,_,_,(Id,X,Y,Dir,M),_,Eles,_,_,Free,_,_,_,_,Dir) :-
	viz(Dir,(X,Y),Viz),
	member(Viz,Free),
	\+ naoSegura((X,Y),Id,M,Eles).

% se parede em frente e pastilhas nem ve-las
pacman104(_,_,_,(_,X,Y,Dir,M),_,Eles,_,_,Free,_,_,_,_,Dec) :-
	findall(D,(viz(D,(X,Y),Viz),member(Viz,Free),\+ naoSegura((X,Y),Id,M,Eles)),L),
	inversa(Dir,Inv),
	aoCalhaPrefiroNaoRegressar(L,Inv,Dec).


% a posicao está fora do meu territorio dado o meu Id
foraTerritorio((X,_),Id) :-
	member(Id,[0,1]),!,
	X >= 0.
foraTerritorio((X,_),_) :-
	X < 0.


% eh pena mas tenho mesmo de regressar
aoCalhaPrefiroNaoRegressar([D],_,D).
aoCalhaPrefiroNaoRegressar(L,Inv,Dec) :-
	select(Inv,L,NL),
	random_member(Dec,NL).

% as inversas dos sentidos
inversa(0,180).
inversa(180,0).
inversa(90,270).
inversa(270,90).

% vizinha a norte (0)
viz(0,(X,Y),(X,NY)) :-
	NY is Y + 1.
% vizinha a sul (180)
viz(180,(X,Y),(X,NY)) :-
	NY is Y - 1.
% vizinha a leste (90)
	viz(90,(X,Y),(NX,Y)) :-
	NX is X + 1.
	% vizinha a oeste (270)
viz(270,(X,Y),(NX,Y)) :-
	NX is X - 1.


naoSegura((X,Y),Id,M,Eles) :-
	M > 0,
	\+ foraTerritorio((X,Y),Id),
	member((_,X,Y,_,_),Eles).

naoSegura((X,Y),Id,_,Eles) :-
	foraTerritorio((X,Y),Id),
	member((_,X,Y,_,0),Eles).


segura(Pos,Id,0,Eles) :-
	\+ foraTerritorio(Pos,Id),!.
segura((X,Y),Id,M,Eles) :-
	\+ foraTerritorio(Pos,Id),!,
	\+ member((_,X,Y,_,_),Eles).

