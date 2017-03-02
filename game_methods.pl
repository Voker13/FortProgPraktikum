:- module(game_methods,[
		empty/1,
		player/1,
		all_notPlayer_WLists/3,
		draw_board/1,
		win_board/1, 
		swap_player/2,
		show_board/1,
		domain/3,
		move/3,
		new_empty_board/3,
		count_P/3,
		indexOf/3,
		lowest_member/3,
		hightest_member/3,
		max/3,
		direct_print_nl/1,
		direct_print/1,
		add_lists_together/3
	]).

:- use_module(library(clpfd), []). 

empty(' '). % unser symbol für 'leer'
player(x). % spieler 'x'
player(o). % spieler 'o'

% erstellt ein neues leeres board, welches Rows hoch und Cols breit ist.
new_empty_board(_,0,[]) :- !.
new_empty_board(Rows,Cols,L) :- empty(X), L=[EmptyList|ReEmptyList], new_x_list(Rows,X,EmptyList), DecCols is Cols-1, new_empty_board(Rows,DecCols,ReEmptyList).

% erstellt eine neue liste voll mit X
new_x_list(0,_,[]) :- !.
new_x_list(Length,X,L) :- L = [X|L2], DecLength is Length-1, new_x_list(DecLength,X,L2).

% setzt in im ersten board in spalte column einen stein des players P und gibt das neue board mit player P zurück
move(state(player(P),board([L|Ls],W)),Column,state(player(P),board(NewBoard,W))) :-  
													move_possible(L),
													length(L,Len), 
													Len > Column,
													Column >= 0,
													get_current_row([L|Ls],Column,SelectedRow),
													index_of_last([L|Ls],SelectedRow,RowIndex),
													swap(SelectedRow,P,Column,NewRow),
													swap([L|Ls],NewRow,RowIndex,NewBoard).

% gibt, anders als indexOf, den index vom letzten passenden 'elem' zurück
index_of_last(List,Elem,Index) :- length(List,Length), invert_list(List,InvertList), indexOf(InvertList,Elem,InvertIndex), Index is Length-InvertIndex-1, !.

% ist wie indexOf nur gibt indexofLast das letzte passende element zurück
% muss mit index=0 und Accu=-1 aufgerufen werden
%index_of_last(List, Elem, Index) :- last1(List, Elem, 0, -1, Index).

%last1([], _, _, Acc, Acc).
%last1([L| Ls], Elem, Index, Acc, R) :- IncIndex is Index + 1, (L == Elem, !, last1(Ls, Elem, IncIndex, Index, R); last1(Ls, Elem, IncIndex, Acc, R)).

% gibt die indizes der liste in einer liste zurück
domain([],_,[]).
domain([L|Ls],X,Lr) :-  \+empty(L), IncX is X+1, domain(Ls,IncX,Lr).
domain([L|Ls],X,Li) :-  empty(L), Li = [X|Lr], IncX is X+1, domain(Ls,IncX,Lr).

% gibt in umgekehrter reihenfolge die ersten N nummern der natürlichen zahlen zurück
first_n_numbers_rev(0,[0]).
first_n_numbers_rev(N,L) :- L=[N|Lr], DecN is N-1, first_n_numbers_rev(DecN,Lr), !.

% tauscht an der stelle 'index' einer liste das element 'elem'
swap([_|Ls],Elem,0,[Elem|Ls]).
swap([L|Ls],Elem,Index,[L|Nl]) :- IndexDec is Index - 1, swap(Ls,Elem,IndexDec,Nl).

% gibt ausgehend von Column die tiefe des bordes an in die ein stein fallen würde
get_current_row([L1],Column,E) 	  		:- empty(X), indexOf(L1,X,Column), E = L1 .
get_current_row([L1,L2|_],Column,E) 	:- empty(X), indexOf(L1,X,Column), (\+ indexOf(L2,X,Column)), E = L1 .
get_current_row([L1,L2|Ls],Column,E) 	:- empty(X), indexOf(L1,X,Column), indexOf(L2,X,Column), get_current_row([L2|Ls],Column,E).

% gibt den anderen player zurück
swap_player(P,NewP) :- P = x, NewP = o.
swap_player(P,NewP) :- P = o, NewP = x.

%gibt an ob das board nur aus leeren listen besteht / ob das board leer ist
empty_board(board([R|[]],_)) :- emptyList(R).
empty_board(board([R|Rs],_)) :- emptyList(R), empty_board(board(Rs,_)).

% gibt an ob eine liste leer ist.
emptyList([X]) :- empty(X).
emptyList([R|Rs]) :- empty(R), emptyList(Rs).

% wendet count_p_in_list auf das board an.. summiert alle ergebnisse zu Erg
count_P([],player(_),0).
count_P([L|Ls],player(P),Erg) :- count_p_in_list(L,player(P),CountOfList), count_P(Ls,player(P),Erest), Erg is Erest + CountOfList, !.

% gibt die anzahl der steine von player p in der liste an
count_p_in_list([],player(_),0).
count_p_in_list([L|Ls],player(P),Erg) :- L = P, count_p_in_list(Ls,player(P),Erest), (Erg is Erest +1), !.
count_p_in_list([L|Ls],player(P),Erg) :- L \= P, count_p_in_list(Ls,player(P),Erg), !.

% gibt an ob der spieler P in einer liste vertreten ist.
p_in_list([L|_],player(P)) :- L = P , !.
p_in_list([_|Ls],player(P)) :- p_in_list(Ls,player(P)).

% wie pretty board --> printet das board schön in die konsole
show_board(board([],_)).
show_board(board([Z|Zs],_)) :- write("|"), showLine(Z), nl, show_board(board(Zs,_)).

% hilfsfunktion für zeilen der show_board funktion
showLine([]).
showLine([L|Ls]) :- write(L), write("|"), showLine(Ls).

% guckt nach ob ein unendschieden vorhanden ist
draw_board(board([L|Ls],W)) :-  (\+ win_board(state(player(_),board([L|Ls],W)))),
								(\+ move_possible(L)).

%guckt die erste zeile durch ob noch ein feld frei ist
%muss mit der ersten zeile aufgerufen werden
move_possible([L|_]) 	:- empty(L), !.
move_possible([_|Ls])	:- move_possible(Ls).

%gibt an ob ein spieler auf dem board gewinnt
win_board(state(player(P),board(Rows,W))) :- win_row(player(P),Rows,W). %guckt reihen durch
win_board(state(player(P),board(Rows,W))) :- clpfd:transpose(Rows,Cols), win_row(player(P),Cols,W). %guckt spalten durch
win_board(state(player(P),board(Rows,W))) :- all_diag_right(Rows,Diags), win_row(player(P),Diags,W). %guckt rechte diagonalen durch
win_board(state(player(P),board(Rows,W))) :- reflect(Rows,Cols), all_diag_right(Cols,Diags), win_row(player(P),Diags,W). %guckt linke diagonalen durch

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

% sind alle WListen die nicht leer sind und in denen der player p nicht mehr drin ist
% sprich: alle möglichen gewinn reihen des anderen players
all_notPlayer_WLists(Board,player(P),Erg) :- 	board_allWLists(Board,L), 
												filter_notPlayer_lists(L,player(P),Ezwischen),  
												filter_notEmpty_lists(Ezwischen,Erg), !.

% filtert eine liste von listen, sodass keine leeren listen mehr enthalten sind
filter_notEmpty_lists([],[]).
filter_notEmpty_lists([L|Ls],Erg) :- emptyList(L), filter_notEmpty_lists(Ls,Erg), !.
filter_notEmpty_lists([L|Ls],Erg) :- Erg = [L|Erest], \+ emptyList(L), filter_notEmpty_lists(Ls,Erest).

% Filtert eine liste von listen, sodass alle listen mit player(P) nicht mehr enthalten sind.
filter_notPlayer_lists([],player(_),[]).
filter_notPlayer_lists([L|Ls],player(P),Erg) :- p_in_list(L,player(P)), filter_notPlayer_lists(Ls,player(P),Erg), !.
filter_notPlayer_lists([L|Ls],player(P),Erg) :- Erg = [L|Erest], \+ p_in_list(L,player(P)), filter_notPlayer_lists(Ls,player(P),Erest).

%% sucht nach allen w-langen listen im board
board_allWLists(board(Rows,W),Erg) :- 	append(E1,E2,Erg),
										append_1_allWLists(board(Rows,W),E1),
										append_2_allWLists(board(Rows,W),E2),
										!.
										
% konkadiniert alle WLIsten der Spalten und Reihen
append_1_allWLists(board(Rows,W),E1) :- append(E_Rows,E_Cols,E1), 
										rows_allWLists(Rows,W,E_Rows),
										clpfd:transpose(Rows,Cols), rows_allWLists(Cols,W,E_Cols),
										!.
									
% konkadiniert alle WListen der Rechts und Linksdiagonalen
append_2_allWLists(board(Rows,W),E2) :- append(E_DiagsRight,E_DiagsLeft,E2), 
										all_diag_right(Rows,DiagsRight), rows_allWLists(DiagsRight,W,E_DiagsRight),
										reflect(Rows,ReflectRows), all_diag_right(ReflectRows,DiagsLeft), rows_allWLists(DiagsLeft,W,E_DiagsLeft),
										!.
										

%%
%rows_allWLists([L],W,Erg) :- allWLists(L,W,Erg).
rows_allWLists([],_,[]).
rows_allWLists([L|Ls],W,Erg) :- append(E1,E2,Erg), allWLists(L,W,E1), rows_allWLists(Ls,W,E2), !.

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
							diag_right(List,0,E1), % oben links angefangen, nach rechts gehend
							clpfd:transpose(List,TList),
							diag_right(TList,1,E2), !. % oben links in der transponierten matrix an pos 1 anfangend, nach rechts gehend

%Index sollte immer 0 oder 1 sein!
% 0 um die diagonalen vom ersten elem. der ersten liste aus zu erreichen
% 1 um die diagonalen vom ersten elem. der restlichen listen aus zu erreichen	
% iteriert mit diag/3 das board durch			
%[[]] -> [[]]
diag_right([L|_],Index,E) :- length(L,Len), Pos is Index+1, Len < Pos, E = [], !. %abbruch wenn index out of bounds
diag_right([L|Ls],Index,E) :- E = [E1|E2],
								diag([L|Ls],Index,E1),
								InkIndex is Index +1,
								diag_right([L|Ls], InkIndex,E2), !.

%gibt diagonale aus
% Bsp: [[1,2,3],[4,5,6],[7,8,9]], 1, [2,6]
% Bsp: [[1,2,3],[4,5,6],[7,8,9]], 0, [1,5,9]
%([[]],int) -> []
diag([],_,E) :- E = [] ,!.
diag([L|_], Pos, E) :- length(L,Len), Index is Pos+1, Len < Index, !, E = []. %abbruch wenn index out of bounds
diag([L|Rows], Pos, E) :- 	E = [Elem|Er],
							indexOf(L,Elem,Pos),
							InkPos is Pos +1,
							diag(Rows,InkPos,Er).
							
%refelcting a matrix on the side
reflect([L],[E]) :- invert_list(L,E), !.
reflect([L|Ls], E) :- 	E = [E1|E2],
						invert_list(L,E1),
						reflect(Ls,E2).
	
% invertiret eine liste
invert_list([],[]).
invert_list([L|Ls],E) :- invert_list(Ls,LsE), append(LsE, [L], E).

% gibt das kleinste elem der liste zurück										
lowest_member([],V,V). 
lowest_member([L|Ls],V,Erg) :- L < V,lowest_member(Ls,L,Erg), !.
lowest_member([L|Ls],V,Erg) :- L >= V, lowest_member(Ls,V,Erg), !.

%gibt das größte elem der liste zurück
hightest_member([],V,V). 
hightest_member([L|Ls],V,Erg) :- L > V, hightest_member(Ls,L,Erg), !.
hightest_member([L|Ls],V,Erg) :- L =< V, hightest_member(Ls,V,Erg), !.

% nimmt 2 listen und addiert L1(i) und L2(i) in ein weitere liste
add_lists_together([],[],[]).
add_lists_together([L1|L1s],[L2|L2s],Lerg) :- Lerg = [E1|Erest], E1 is L1+L2, add_lists_together(L1s,L2s,Erest).

% returns the max value of N and M
max(N,M,N) :- N >= M .
max(N,M,M) :- M > N .

% prints instantly the String wothout prolog resolution
direct_print(String) :- write(String).

% prints instantly the String and a next line wothout prolog resolution
direct_print_nl(String) :- write(String), nl.
					
% liste - elem - index					
indexOf([Elem|_],Elem,0).
indexOf([_|Ls],Elem,Index) :- indexOf(Ls,Elem,Iink), Index is Iink+1.
