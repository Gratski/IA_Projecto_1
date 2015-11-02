


% insere os filhos á cabeça das sub-listas
insert_all( [], _, Acc, Acc ).
insert_all( [ P | R ], L, Acc, Res ):-
	append( [P], L, NL ),
	insert_all( R, L, [ NL | Acc ], Res ).


%pacman(Clock, ClockLimit, Score, Me, Partner, OtherTeam, HomeBase , HisBase, FreeCells, MyP, MYSupP, HisP, HisSupP, Decisao) :-

pacman0(_,_,_,(0,X,Y,_,_),_,_,_,_,Free,_,_,_,_,Dec) :-
	findall(D,(viz(D,(X,Y),Viz),member(Viz,Free)),L),
	random_member(Dec,L).

pacman0(_,_,_,(0,X,Y,_,_),_,_,_,_,Free,_,_,Pastilhas,_,Dec):-
	get_best_dir( [[ (X, Y) ]], Free, Pastilhas, Dec ).

%get_best_dir( [ [Branch], ..., [Branch] ], Free, Pastilhas_Adv , Direcao )
get_best_dir( [ ],_, _, Direction ):-
	Direction is 270.

get_best_dir( [ [ ( X, Y ) | R] | RT ], _, Pastilhas, Direction ):-
	member( (X, Y), Pastilhas ),
	write([(X, Y) | R]), nl,
	Direction is 0.

get_best_dir( [ [ ( X, Y ) | R] | RT ], Free, Gums, Direction ):-
	findall( Viz, ( viz( D, ( X, Y ), Viz), member( Viz, Free ), \+member( Viz, [ ( X, Y ) | R ] ) ), L),
	apply_heuristic( L, Gums, H ),
	insert_all( L, [ (X, Y) | R ], [], NL ),
	append( NL, RT, NNL ),
	get_best_dir( NNL, Free, Gums, Direction ).


apply_heuristic( [P | R], Gums,  )


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get_best_dir( [[(1,1)]], [ (0,0), (0,1), (0,2), (0,3), (1,0), (1,1), (1,2), (1,3), (2,0), (2,1), (2,2), (2,3), (3,0), (3,1), (3,2), (3,3) ], [(1,2)], D ).


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
