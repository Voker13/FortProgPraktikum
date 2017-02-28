:- module(game_logic,[
		random_logic/2,
		logic_01/2,
		logic_02/2
	]).

:- use_module(game_methods, [
		all_notPlayer_WLists/3,
		swap_player/2,
		move/3,
		indexOf/3,
		lowest_member/3,
		hightest_member/3,
		domain/3,
		win_board/1,
		count_P/3
	]).
	
random_logic(game(state(_,board([L|_],_)),_,_,_),SelectedColumn) :- domain(L,0,Dom), random_member(SelectedColumn,Dom).

logic_02(game(state(player(P),board([L|Ls],W)),_,_,_),SelectedColumn) :- 
	domain(L,0,Domain),
	funclvl1(Domain, state(player(P),board([L|Ls],W)), NewDomain),
	random_member(SelectedColumn,NewDomain).
	
% gibt nur domains zurück, wenn der gegner danach nicht gewinnen kann! (einen zug vorraus berechnen)
funclvl1([],_,[]).
funclvl1([D|Ds], CurrentState, NewDomain) :- 
	NewDomain = [D|NewDomainRest],
	move(CurrentState,D,state(player(P),board([L|Ls],W))),
	domain(L,0,Domain),
	swap_player(P, Enemy),
	\+ funclvl2(Domain, state(player(Enemy),board([L|Ls],W))), % gegner kann nicht in D nächste runde gewinnen
	funclvl1(Ds, CurrentState, NewDomainRest).
	
funclvl1([D|Ds], CurrentState, NewDomain) :- 
	move(CurrentState,D,state(player(P),board([L|Ls],W))),
	domain(L,0,Domain),
	swap_player(P, Enemy),
	funclvl2(Domain, state(player(Enemy),board([L|Ls],W))), % gegner kann in D nächste runde gewinnen
	funclvl1(Ds, CurrentState, NewDomain).

%% überprüft ob mit den zügen(Ds) im state(CurrentState) gewonnen werden kann
funclvl2([D|_], CurrentState) :- move(CurrentState,D,NextState), win_board(NextState).
funclvl2([D|Ds], CurrentState) :- move(CurrentState,D,NextState), \+ win_board(NextState), funclvl2(Ds,CurrentState), !.

logic_01(game(state(player(P),board([L|Ls],W)),_,_,_),SelectedColumn) :- 
	domain(L,0,Domain),
	swap_player(P,Enemy),% write(Dimension),
	iter1(Domain, P, Enemy, state(player(P),board([L|Ls],W)), ValueList), nl, write(ValueList),
	lowest_member(ValueList,100000,Lowest),
	indexOf(ValueList,Lowest,IndexVList),
	indexOf(Domain,SelectedColumn,IndexVList), !. 
	
iter1([], _, _, _, []). 
iter1([D|Ds], P, Enemy, CurrentState, Erg) :- 
	Erg = [N|Erest],
	move(CurrentState, D, state(player(_), NewBoard)),
	all_notPlayer_WLists(NewBoard,player(P),WLists), nl, write(D), write(" : "), write(WLists),
	count_P(WLists,player(Enemy),N),%sst  <-- wertung
	iter1(Ds, P, Enemy, CurrentState, Erest), !.
	
count_empty([],player(_),0).
count_empty([L|Ls],player(P),Erg) :- 
	count_empty_in_list(L,player(P),CountOfList), 
	count_empty(Ls,player(P),Erest), 
	Erg is Erest + (CountOfList*CountOfList), !.

count_empty_in_list([],player(_),0).
count_empty_in_list([L|Ls],player(P),Erg) :- empty(L), count_empty_in_list(Ls,player(P),Erest), (Erg is Erest +1), !.
count_empty_in_list([L|Ls],player(P),Erg) :- \+ empty(L), count_empty_in_list(Ls,player(P),Erg), !.