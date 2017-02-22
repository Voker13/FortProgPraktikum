:- use_module(library(clpfd), []).

empty("_").
player(x).
player(o).

empty_board(board([R|[]],_)) :- emptyList(R).
empty_board(board([R|Rs],_)) :- emptyList(R), empty_board(board(Rs,_)).

emptyList(["_"]).
emptyList([R|Rs]) :- empty(R), emptyList(Rs).

show_board(board([],_)).
show_board(board([Z|Zs],_)) :- write("|"), showLine(Z), nl, show_board(board(Zs,_)).

showLine([]).
showLine([L|Ls]) :- write(L), write("|"), showLine(Ls).

%gibt an ob ein spieler auf dem board gewinnt
win_board(player(P),board(Rows,integer(W))) :- win_row(player(P),board(Rows,integer(W))). %guckt reihen durch
win_board(player(P),board(Rows,integer(W))) :- clpfd:transpose(Rows,Cols), win_row(player(P),board(Cols,integer(W))). %guckt spalten durch
%win_board(player(P),board(Rows,integer(W))) :- 

%gibt an ob ein spieler in irgendeiner reihe gewinnt
%(player,board) -> boolean
win_row(player(P),board([L|_],integer(W))) :- 	allWLists(L,integer(W),El), win_line(player(P),El). %in erser reihe gewonnen
win_row(player(P),board([_|Ls],integer(W))) :- 	win_row(player(P),board(Ls,integer(W))). %in restlichen reihen gewonnen

%gibt an ob ein spieler in einer linie gewinnt (MUSS mit allWLists bedient werden!!!) 
%(player, [[]]) -> boolean
win_line(player(P),[L|Ls]) :- win_List(player(P),L); win_line(player(P),Ls).

%gibt an ob ein spieler durch eine liste gewinnt
%(player, []) -> boolean
win_List(_,[]).
win_List(player(P),[L|Ls]) :- P = L, win_List(player(P),Ls).

%gibt alle Teillisten aus, die W-lang sind, wobei die reihenfolge eingehalten wird( nicht alle Permutationen)
%([],int) -> [[]]
allWLists([L|Ls],integer(W),Erg) :- length([L|Ls],Len), Len < W, Erg = [].
allWLists([L|Ls],integer(W),Erg) :- Erg = [E1|Er], 
									wList([L|Ls],integer(W),E1),
									allWLists(Ls,integer(W),Er).

%gibt die ersten W stellen einer Liste als Liste aus.
%([], int) -> []
wList(_,integer(W),Erg) :- integer(W) = integer(0) , Erg = [].
wList([L|Ls],integer(W),Erg) :- Erg = [L|Lr], W2 is W - 1, wList(Ls,integer(W2),Lr).

%listet alle rechtsdiagonalen einer liste von listen
%[[]] -> [[]]
all_diag_right(List,E) :- 	%E = [E1|E2],
							append(E1,E2,E),
							diag_right(List,0,E1),
							clpfd:transpose(List,TList),
							diag_right(TList,1,E2), !.

%Index sollte immer 0 oder 1 sein!
% 0 um die diagonalen vom ersten elem. der ersten liste aus zu erreichen
% 1 um die diagonalen vom ersten elem. der restlichen listen aus zu erreichen				
%[[]] -> [[]]
diag_right(List,Index,E) :- length(List,Len), Pos is Index+1, Len < Pos, E = [], !. %abbruch wenn index out of bounds
diag_right(List,Index,E) :- E = [E1|E2],
								diag(List,Index,E1),
								InkIndex is Index +1,
								diag_right(List, InkIndex,E2).

%gibt diagonale aus
%([[]],int) -> []
diag([],_,E) :- E = [] ,!.
diag([L|_], Pos, E) :- length(L,Len), Index is Pos+1, Len < Index, !, E = []. %abbruch wenn index out of bounds
diag([L|Rows], Pos, E) :- 	E = [Elem|Er],
							indexOf(L,Elem,Pos),
							InkPos is Pos +1,
							diag(Rows,InkPos,Er).
										
										
indexOf([Elem|_],Elem,0).
indexOf([_|Ls],Elem,Index) :- indexOf(Ls,Elem,Iink), Index is Iink+1.
