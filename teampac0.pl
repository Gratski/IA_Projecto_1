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


% pacman com ID 0
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pacman0(_,_,_,(0,X,Y,Dir,_),_,_,_,_,Free,_,_,Pastilhas,_,Dec):-
	get_best_dir( [[ (X, Y, Dir) ]], Free, Pastilhas, Dec ).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Metodos auxiliares


% obtem o melhor caminho em depth
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_best_dir( [ ], Free, Gums, Direction ):-
	Direction is 270.

get_best_dir( [ [ ( X, Y, Dir ) | R] | RT ], Free, Gums, Direction ):-
	member( (X, Y), Gums ),
	Direction is 90.

get_best_dir( [ [ ( X, Y, Dir ) | R] | RT ], Free, Gums, Direction ):-
	findall( ( Nx, Ny, D ) , ( viz( D, ( X, Y ), (Nx, Ny) ), \+member( ( Nx, Ny ) , [ ( X, Y ) | R ] ) , member( ( Nx, Ny ) , Free ) ), L),
	insert_all( L, [ (X, Y, Dir) | R ], [], NL ),
	append( NL, RT, NNL ),
	get_best_dir( NNL, Free, Gums, Direction ).	



% insere os filhos á cabeça das sub-listas
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
insert_all( [], _, Acc, Acc ).
insert_all( [ P | R ], L, Acc, Res ):-
	append( [P], L, NL ),
	insert_all( R, L, [ NL | Acc ], Res ).


viz(0,(X,Y),(X,NY)) :-
	NY is Y + 1.
viz(180,(X,Y),(X,NY)) :-
	NY is Y - 1.
viz(90,(X,Y),(NX,Y)) :-
	NX is X + 1.
viz(270,(X,Y),(NX,Y)) :-
	NX is X - 1.



