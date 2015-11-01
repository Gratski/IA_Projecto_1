
%%%%%%%%%%%%
% Comportamento ZigZag Pacman
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module(library(lists)).


%pacman(Clock, ClockLimit, Score, Me, Partner, OtherTeam, HomeBase , HisBase, FreeCells, MyP, MYSupP, HisP, HisSupP, Decisao) :-

pacman0(_,_,_,(_,X,Y,_,_),_,_,_,_,Free,_,_,_,_,Dec) :-
	findall(D,(viz(D,(X,Y),Viz),member(Viz,Free)),L),
	random_member(Dec,L).

viz(0,(X,Y),(X,NY)) :-
	NY is Y + 1.
viz(180,(X,Y),(X,NY)) :-
	NY is Y - 1.
viz(90,(X,Y),(NX,Y)) :-
	NX is X + 1.
viz(270,(X,Y),(NX,Y)) :-
	NX is X - 1.

% MEU
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pacman0(_,_,_,(_,X,Y,D,_),_,_,_,_,Free,_,_,Gums,MaxGums,Dec) :-
	get_dir( [ [ ( X, Y, D ) ] ], Gums, MaxGums, Free, Dec ).


get_dir( [ [ (X, Y, D) | IR ] | OR ], Gums, MaxGums, Navs , Direction):-
	member( (X, Y) , Gums ),
	member( (X, Y), Navs ),
	find_direction([ (X, Y, D) | IR ], Direction).

get_dir( [ [ ( X, Y, Dir ) | IR ] | OR ], Gums, MaxGums, Navs, Direction):-
	findall( (Nx, Ny, D), ( viz(D,(X, Y), (Nx, Ny)), member((Nx, Ny), Navs), \+member( (Nx, Ny), [ ( X, Y, Dir ) | IR ] ) ), L),

add_new_elems( [], _, Acc, Acc ).
add_new_elems( [ P | R ], L, Acc, Res ):-
	append(P, L, NL),
	add_new_elems( R, L, [NL | Acc], Res ).

find_direction( [ (_, _, D) ], D ).
find_direction( [ _ | R ], D ):-
	find_direction( R, D).




