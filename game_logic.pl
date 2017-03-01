:- module(game_logic,[
		random_logic/2,
		logic_01/2,
		logic_02/2,
		logic_03/2
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
		add_lists_together/3,
		direct_print_nl/1,
		direct_print/1,
		count_P/3
	]).
	
random_logic(game(state(_,board([L|_],_)),_,_,_),SelectedColumn) :- domain(L,0,Dom), random_member(SelectedColumn,Dom).

logic_02(game(state(player(P),board([L|Ls],W)),_,_,_),SelectedColumn) :- 
	domain(L,0,Domain),
	funclvl1(Domain, state(player(P),board([L|Ls],W)), NewDomain),
	random_member(SelectedColumn,NewDomain).
	
% KI kann gewinnen!
logic_03(game(state(player(P),board([L|Ls],W)),_,_,_),SelectedColumn) :- 
	domain(L,0,Domain),
	select_win(Domain,state(player(P),board([L|Ls],W)),SelectedColumn).
	
% gegner wird auf jeden fall gewinnen
logic_03(game(state(player(P),board([L|Ls],W)),_,_,_),SelectedColumn) :- 
	domain(L,0,Domain),
	\+ select_win(Domain,state(player(P),board([L|Ls],W)),_),
	exclude_enemys_win(Domain, state(player(P),board([L|Ls],W)), BetterDomain),% nl, write(BetterDomain),
	BetterDomain = [],
	random_member(SelectedColumn,Domain).
	
logic_03(game(state(player(P),board([L|Ls],W)),_,_,_),SelectedColumn) :- 
	domain(L,0,Domain),
	\+ select_win(Domain,state(player(P),board([L|Ls],W)),_),
	exclude_enemys_win(Domain, state(player(P),board([L|Ls],W)), BetterDomain),% nl, write("deny user win: "), write(BetterDomain),
	\+ BetterDomain = [],
	bewertung_1(BetterDomain,state(player(P),board([L|Ls],W)),ErgBew1),% nl, write(ErgBew1),
	bewertung_2(BetterDomain,state(player(P),board([L|Ls],W)),ErgBew2),% nl, write(ErgBew2),
	add_lists_together(ErgBew1,ErgBew2,BewList),% nl, write(BewList),
	hightest_member(BewList,-100000,Hightest),% nl, write("hightest: "), write(Hightest),
	indexOf(BewList,Hightest,IndexVList),
	indexOf(BetterDomain,SelectedColumn,IndexVList). 
	
% gibt eine bewertung zu jeder column (Ds) in der Liste Erg aus 
% // bewertung_1 kümmert sich um das füllen eigener listen
bewertung_1([],_,[]).
bewertung_1([D|Ds],state(player(P),Board),Erg) :-
	Erg = [E|Erest], 
	swap_player(P,Enemy),
	length(WLists1,Len1),
	zero_to_one(Len1,Len1not0),
	all_notPlayer_WLists(Board,player(Enemy),WLists1),% nl,write("L1: "), write(WLists1),
	move(state(player(P),Board),D,state(player(_),NewBoard)),
	length(WLists2,Len2),
	zero_to_one(Len2,Len2not0),
	all_notPlayer_WLists(NewBoard,player(Enemy),WLists2),%nl, write("L2: "), write(WLists2),nl,
	count_P(WLists1,player(P),Count1),
	count_P(WLists2,player(P),Count2),
	E is round((float(Count2/Len2not0)-float(Count1/Len1not0))*1000), % faktor gibt die wertung gegenüber bewertung_2 an!
	bewertung_1(Ds,state(player(P),Board),Erest), !.
	
% gibt eine bewertung zu jeder column (Ds) in der Liste Erg aus 
% // bewertung_2 kümmert sich um das BLOCKEN DES GEGNERS!
bewertung_2([],_,[]).
bewertung_2([D|Ds],state(player(P),Board),Erg) :- 
	Erg = [E|Erest],
	length(WLists1,Len1),
	all_notPlayer_WLists(Board,player(P),WLists1),% nl,write("L1: "), write(WLists1),
	move(state(player(P),Board),D,state(player(_),NewBoard)),
	length(WLists2,Len2),
	all_notPlayer_WLists(NewBoard,player(P),WLists2),%nl, write("L2: "), write(WLists2),nl,
	E is (Len1-Len2)*100,
	bewertung_2(Ds,state(player(P),Board),Erest), !.
	
% wählt ersten zug aus mit dem gewonnen werden kann
select_win([D|_], CurrentState, D) :-
	move(CurrentState,D,NewState),
	win_board(NewState). 

select_win([D|Ds], CurrentState, E) :-
	move(CurrentState,D,NewState),
	\+ win_board(NewState),
	select_win(Ds, CurrentState, E).
	
% gibt nur domains zurück, wenn der gegner danach nicht gewinnen kann! (einen zug vorraus berechnen)
exclude_enemys_win([],_,[]).
exclude_enemys_win([D|Ds], CurrentState, NewDomain) :- 
	NewDomain = [D|NewDomainRest],
	move(CurrentState,D,state(player(P),board([L|Ls],W))),
	domain(L,0,Domain),
	swap_player(P, Enemy),
	\+ funclvl2(Domain, state(player(Enemy),board([L|Ls],W))), % gegner kann nicht in D nächste runde gewinnen
	exclude_enemys_win(Ds, CurrentState, NewDomainRest), !.
	
exclude_enemys_win([D|Ds], CurrentState, NewDomain) :- 
	move(CurrentState,D,state(player(P),board([L|Ls],W))),
	domain(L,0,Domain),
	swap_player(P, Enemy),
	funclvl2(Domain, state(player(Enemy),board([L|Ls],W))), % gegner kann in D nächste runde gewinnen
	exclude_enemys_win(Ds, CurrentState, NewDomain), !.

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

zero_to_one(N,N) :- N \= 0.
zero_to_one(N,E) :- N = 0, E=1.