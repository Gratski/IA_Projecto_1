:- dynamic last_pos/2.

pacman124(1,_,_,(Id,PacX,PacY,_,_),_,_,_,_,_,_,_,_,_,90) :-
	retractall(last_pos(Id,_)),
	assert(last_pos(Id,(PacX,PacY))),!.

pacman124(_,_,_,(Id,PacX,PacY,_,_),_,_,_,_,_,_,_,_,_,270) :-
	last_pos(Id, (X, Y)),
	retractall(last_pos(Id,_)),
	assert(last_pos(Id,(PacX,PacY))),!.


viz(0,(X,Y),(X,NY)) :-
	NY is Y + 1.
viz(180,(X,Y),(X,NY)) :-
	NY is Y - 1.
viz(90,(X,Y),(NX,Y)) :-
	NX is X + 1.
viz(270,(X,Y),(NX,Y)) :-
	NX is X - 1.