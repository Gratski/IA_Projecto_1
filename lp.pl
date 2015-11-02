%
insere_em_vector( (X, Y), [], [(X, Y)]).
insere_em_vector( (X, Y), [ (Px, Py) | R ], Res):-
	X == Px, Y == Py,
	Res = [ (Px, Py) | R ].
insere_em_vector( (X, Y), [ (Px, Py) | R ], Res):-
	( X \= Px ; Y \= Py ),
	insere_em_vector( (X, Y), R, Res2 ),
	Res = [ (Px, Py) | Res2 ].

get_dir( (PacX, PacY), (ObjX, ObjY), [ [ (X, Y) | IR ] | OR ], Free, Visitados, Gums, Dec ):-
	X == ObjX, Y == ObjY,
	reverse( [ (X, Y) | IR ], [ _, (VizX, VizY) | VizR ] ),
	viz(Dec, (X, Y), (VizX, VizY)).

get_dir( (PacX, PacY), (ObjX, ObjY), [ [ (X, Y) | IR ] | OR ], Free, Visitados, Gums, Dec )
	