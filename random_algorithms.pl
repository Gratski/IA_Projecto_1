%%%%
%insert considerind current position and heuristic method
%%
% base case
insert_with_heuristic( (Cost, Tree), [], [ Cost, Tree ] ).

% do not insert yet because it is still bigger
insert_with_heuristic( (Cost_1, [ (X1, Y1) | R1 ]), [ ( Cost_2, [ (X2, Y2) | R2 ] ) | R ], L ):-
	heuristic( (X1, Y1), H_1 ), heuristic( (X2, Y2), H_2 ),
	( H_1 > H_2 ),
	insert_with_heuristic( (Cost_1, [ (X1, Y1) | R1 ]), R, NL ),
	L = [ ( Cost_2, [ (X2, Y2) | R2 ] | NL ].

% insert here
insert_with_heuristic( (Cost_1, [ (X1, Y1) | R1 ]), [ ( Cost_2, [ (X2, Y2) | R2 ] ) | R ], L ):-
	heuristic( (X1, Y1), H_1 ), heuristic( (X2, Y2), H_2 ),
	( (H_1 < H_2) ; ( H_1 == H_2 ) ),
	L = [ (Cost_1 , [ (X1, Y1) | R1 ]), (Cost_2, [ (X2, Y2) | R2 ]) | R ].
%%%%

%%%%
% sort using heuristic method
%%
% base case
sort_a_star( [], Acc, Acc ).

% insert all elements
sort_a_star( [ (Cost, Tree) | R ], Acc, L ):-
	insert_with_heuristic( (Cost, Tree), Acc, NewAcc ),
	sort_a_star( R, NewAcc, L ).

%%%%
% A* algorithm with (x, y) coordinates system
%%
% base case
a_star( [ (C, [ ( X, Y ) | R ] ) | OR ], GumList, Decision ):-
	member( (X, Y), GumList ),
	reverse( [ ( X, Y) | R ], [ (Fx, Fy), (Sx, Sy) | Rev ] ),
	viz( Decision, (Fx, Fy), (Sx, Sy) ).

% go to better option
a_star( [ (C, [ ( X, Y ) | R ] ) OR ], GumList, Decision ):-
	sucs( (X, Y), Sucs ),
	NewCost is C + 1,
	insertCost( NewCost, Sucs, SucsWithCost ),
	insert_all_with_cost( SucsWithCost, [ ( X, Y ) | R ], AllWithCost ),
	append( AllWithCost, OR, AllToSort ),
	sort_a_star( AllToSort, [], Sorted ),
	a_star( Sorted, GumList, Decision ).













