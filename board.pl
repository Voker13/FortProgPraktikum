:- use_module(library(clpfd), []).

empty(' ').
player(x).
player(o).
human(x).
human(o).

initialize(game(state(player(x),board(NewBoard,W)),Human,Rows,Cols)) :- 	write("Rows:"),read(Rows),
														write("Cols:"),read(Cols),
														write("W:"),read(W),
														write("P:"),read(Human),
														new_empty_board(Rows,Cols,NewBoard),
														show_board(board(NewBoard,W)),
														turn(game(state(player(x),board(NewBoard,W)),Human,Rows,Cols),_).

new_empty_board(_,0,[]) :- !.
new_empty_board(Rows,Cols,L) :- empty(X), L=[EmptyList|ReEmptyList], new_x_list(Rows,X,EmptyList), DecCols is Cols-1, new_empty_board(Rows,DecCols,ReEmptyList).

new_x_list(0,_,[]) :- !.
new_x_list(Length,X,L) :- L = [X|L2], DecLength is Length-1, new_x_list(DecLength,X,L2).

move(state(player(P),board([L|Ls],W)),Column,state(player(P),board(NewBoard,W))) :-  
													move_possible(L),
													length(L,Len), 
													Len > Column,
													Column >= 0,
													get_current_row([L|Ls],Column,SelectedRow),
													indexOf([L|Ls],SelectedRow,RowIndex),
													swap(SelectedRow,P,Column,NewRow),
													swap([L|Ls],NewRow,RowIndex,NewBoard).

% humans turn - not won
turn(game(state(player(P),board(B,W)),Human,Rows,Cols),game(state(player(NewP),NewBoard),Human,Rows,Cols)) :-	    player(P) = player(Human),
																				show_board(board(B,W)),
																				write("Next move (0-"),write(Cols),write(")"),
																				read(Column),
																				move(state(player(P),board(B,W)),Column,state(player(P),NewBoard)),																		
																				\+ win_board(player(P),NewBoard),
																				\+ draw_board(NewBoard),
																				swap_player(P,NewP),
																				turn(game(state(player(NewP),NewBoard),Human,Rows,Cols),_).
	
% KI turn - not won																			
turn(game(state(player(P),board([L|Ls],W)),Human,Rows,Cols),game(state(player(NewP),NewBoard),Human,Rows,Cols)) :-	player(P) \= player(Human),
																												show_board(board([L|Ls],W)),
																												dimension(L,0,Dim),
																												random_member(Column,Dim), 
																												move(state(player(P),board([L|Ls],W)),Column,state(player(P),NewBoard)),																	
																												\+ win_board(player(P),NewBoard),
																												\+ draw_board(NewBoard),
																												swap_player(P,NewP),
																												turn(game(state(player(NewP),NewBoard),Human,Rows,Cols),_).

turn(game(state(player(P),board(B,W)),Human,Rows,Cols),game(state(player(_),NewBoard),Human,Rows,Cols)) :-	    player(P) = player(Human),
																				write("Next move (0-"),write(Cols),write(")"),
																				read(Column),
																				move(state(player(P),board(B,W)),Column,state(player(P),NewBoard)),																		
																				win_board(player(P),NewBoard),
																				show_board(NewBoard),
																				write("Human wins!").
																				
turn(game(state(player(P),board([L|Ls],W)),Human,Rows,Cols),game(state(player(_),NewBoard),Human,Rows,Cols)) :-	player(P) \= player(Human),
																												dimension(L,0,Dim),
																												random_member(Column,Dim), 
																												move(state(player(P),board([L|Ls],W)),Column,state(player(P),NewBoard)),																	
																												win_board(player(P),NewBoard),
																												show_board(NewBoard),
																												write("KI wins!").

%draw																				
turn(game(state(player(P),board([L|Ls],W)),Human,Rows,Cols),game(state(player(_),NewBoard),Human,Rows,Cols)) :-	dimension(L,0,Dim),
																												random_member(Column,Dim), 
																												move(state(player(P),board([L|Ls],W)),Column,state(player(P),NewBoard)),																	
																												draw_board(NewBoard),
																												show_board(NewBoard),
																												write("Draw!").

dimension([],_,[]).
dimension([L|Ls],X,Lr) :-  \+empty(L), IncX is X+1, dimension(Ls,IncX,Lr).
dimension([L|Ls],X,Li) :-  empty(L), Li = [X|Lr], IncX is X+1, dimension(Ls,IncX,Lr).

first_n_numbers_rev(0,[0]) :- !.
first_n_numbers_rev(N,L) :- DecN is N-1, L=[N|Lr], first_n_numbers_rev(DecN,Lr).

swap([_|Ls],Elem,0,[Elem|Ls]).
swap([L|Ls],Elem,Index,[L|Nl]) :- IndexDec is Index - 1, swap(Ls,Elem,IndexDec,Nl).

get_current_row([L1],Column,E) 	  		:- empty(X), indexOf(L1,X,Column), E = L1 .
get_current_row([L1,L2|_],Column,E) 	:- empty(X), indexOf(L1,X,Column), (\+ indexOf(L2,X,Column)), E = L1 .
get_current_row([L1,L2|Ls],Column,E) 	:- empty(X), indexOf(L1,X,Column), indexOf(L2,X,Column), get_current_row([L2|Ls],Column,E).

swap_player(P,NewP) :- P = x, NewP = o.
swap_player(P,NewP) :- P = o, NewP = x.

empty_board(board([R|[]],_)) :- emptyList(R).
empty_board(board([R|Rs],_)) :- emptyList(R), empty_board(board(Rs,_)).

emptyList([X]) :- empty(X).
emptyList([R|Rs]) :- empty(R), emptyList(Rs).

show_board(board([],_)).
show_board(board([Z|Zs],_)) :- write("|"), showLine(Z), nl, show_board(board(Zs,_)).

showLine([]).
showLine([L|Ls]) :- write(L), write("|"), showLine(Ls).

draw_board(board([L|Ls],W)) :-  (\+ win_board(player(_),board([L|Ls],W))),
								(\+ move_possible(L)).

%guckt die erste zeile durch ob noch ein feld frei ist
%muss mit der ersten zeile aufgerufen werden
move_possible([L|_]) 	:- empty(L), !.
move_possible([_|Ls])	:- move_possible(Ls).

%gibt an ob ein spieler auf dem board gewinnt
win_board(player(P),board(Rows,W)) :- win_row(player(P),Rows,W). %guckt reihen durch
win_board(player(P),board(Rows,W)) :- clpfd:transpose(Rows,Cols), win_row(player(P),Cols,W). %guckt spalten durch
win_board(player(P),board(Rows,W)) :- all_diag_right(Rows,Diags), win_row(player(P),Diags,W). %guckt rechte diagonalen durch
win_board(player(P),board(Rows,W)) :- reflect(Rows,Cols), all_diag_right(Cols,Diags), win_row(player(P),Diags,W).

%gibt an ob ein spieler in irgendeiner reihe gewinnt
%(player,board) -> boolean
win_row(player(P),[L|_],W) :- 	allWLists(L,W,El), win_line(player(P),El). %in erser reihe gewonnen
win_row(player(P),[_|Ls],W) :- win_row(player(P),Ls,W). %in restlichen reihen gewonnen

%gibt an ob ein spieler in einer linie gewinnt (MUSS mit allWLists bedient werden!!!) 
%(player, [[]]) -> boolean
win_line(player(P),[L|Ls]) :- win_List(player(P),L); win_line(player(P),Ls).

%gibt an ob ein spieler durch eine liste gewinnt
%(player, []) -> boolean
win_List(_,[]).
win_List(player(P),[L|Ls]) :- P = L, win_List(player(P),Ls).

%gibt alle Teillisten aus, die W-lang sind, wobei die reihenfolge eingehalten wird( nicht alle Permutationen)
%([],int) -> [[]]
allWLists([L|Ls],W,Erg) :- length([L|Ls],Len), Len < W, Erg = [].
allWLists([L|Ls],W,Erg) :- Erg = [E1|Er], 
									wList([L|Ls],W,E1),
									allWLists(Ls,W,Er).

%gibt die ersten W stellen einer Liste als Liste aus.
%([], int) -> []
wList(_,W,Erg) :- W = 0 , Erg = [].
wList([L|Ls],W,Erg) :- Erg = [L|Lr], W2 is W - 1, wList(Ls,W2,Lr).

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
							
%refelcting a matrix on the side
reflect([L],[E]) :- invert_list(L,E).
reflect([L|Ls], E) :- 	E = [E1|E2],
						invert_list(L,E1),
						reflect(Ls,E2).
	
invert_list([],[]).
invert_list([L|Ls],E) :- invert_list(Ls,LsE), append(LsE, [L], E).
										
										
indexOf([Elem|_],Elem,0).
indexOf([_|Ls],Elem,Index) :- indexOf(Ls,Elem,Iink), Index is Iink+1.
