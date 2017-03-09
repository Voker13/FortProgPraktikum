:- module(game_logic,[
		random_logic/2,
		logic_01/2,
		logic_02/2,
		logic_03/2,
		logic_tree/2
	]).

:- use_module(game_methods, [
		all_notPlayer_WLists/3,
		swap_player/2,
		move/3,
		indexOf/3,
		lowest_member/2, 
		hightest_member/2,
		domain/3,
		win_board/1,
		add_lists_together/3,
		direct_print_nl/1,
		direct_print/1,
		show_board/1,
		max/3,
		min/3,
		count_P/3
	]).
	
% logic_tree(tiefe,state,Zug,Bewertung)
logic_tree(game(state(P,board([L|Ls],W)),_,_,_),SelectedColumn) :-
	Tiefe is 1,
	domain(L,0,Domain),
	start_tree(Tiefe,Domain,state(P,board([L|Ls],W)),BewList), write("Bewertung: "), write(BewList), nl,
	hightest_member(BewList,Hightest),
	indexOf(BewList,Hightest,Index),
	indexOf(Domain,SelectedColumn,Index),
	!. % wenn kein cut, dann gibt er noch andere mögliche züge aus, wenn gleiche wertung
	
start_tree(_,[],_,[]).
start_tree(Tiefe,[D|Ds],State,BewList) :-
	BewList = [Wert|RestWerte],
	%write("D: "),write(D),nl,
	own_node(Tiefe,[D],State,Wert),
	%write("Wert: "),write(Wert),nl,
	start_tree(Tiefe,Ds,State,RestWerte), 
	!. 
	
own_node(_,[],_,-100000) :- !. % keine weiteren fälle in dieser Tiefe <<<<<-------- darf bei max nicht rauskommen
own_node(0,[D|Ds],State,Bewertung) :- % Tiefe erreicht und kein sieg --> heuristik greift für kinder
	\+ select_win([D],State,_),
	%write("own_node     >>> Tiefe: "), write(0),write(", Column: "),write(D),write(" -- depth reached -- heuristik -- return 0 -- "),write(State),nl,
	move(State,D,state(player(P),board([L|Ls],W))),
	domain(L,0,NewDomain),
	swap_player(P,Enemy),
	bewertung_1(NewDomain,state(player(Enemy),board([L|Ls],W)),BewList1),
	bewertung_2(NewDomain,state(player(Enemy),board([L|Ls],W)),BewList2),
	add_lists_together(BewList1,BewList2,BewListUnited),
	hightest_member(BewListUnited,Lowest),  % nimmt den besten zu für den gegner 
	Wert is Lowest*(-1),              % da aber hier grade "mini" ist muss der wert negiert werden --> also hat er den min zug gewählt
	%write("Wert: "), write(Wert), write("; BewList1: "), write(BewList1), write("; BewList2: "), write(BewList2), write("; BewListUnited: "), write(BewListUnited), nl,
	own_node(0,Ds,State,WertRest),
	max(Wert,WertRest,Bewertung),
	!.  % VORERST HIER >>> 0 <<<
own_node(Tiefe,[D|Ds],State,Bewertung) :- % fall dass man gewinnt
	select_win([D],State,_),
	%write("own_node     >>> Tiefe: "), write(Tiefe),write(", Column: "),write(D),write(" -- win -- "),write(State),nl,
	own_node(Tiefe,Ds,State,WertRest),
	max(1000,WertRest,Bewertung),
	!.  % <-- wertsteigerung bei einem möglichen Sieg (Bewertung = 1 + WertRest)
own_node(Tiefe,[D|Ds],State,Bewertung) :- % kein sieg und tiefe noch nicht erreicht
	Tiefe > 0,
	\+ select_win([D],State,_),
	%write("own_node     >>> Tiefe: "), write(Tiefe),write(", Column: "),write(D),write(" -- no win -- not deep enought -- "),write(State),nl,
	move(State,D,state(player(P),board([L|Ls],W))),
	domain(L,0,EnemyDomain),
	enemy_node(Tiefe,EnemyDomain,state(player(P),board([L|Ls],W)),Wert),  %  <<<--- bleibt hier stecken -.- 
	own_node(Tiefe,Ds,State,WertRest),
	max(Wert,WertRest,Bewertung),
	!.
	
enemy_node(_,[],_,100000) :- !. % keine weiteren fälle in dieser Tiefe
enemy_node(Tiefe,[D|Ds],state(player(P),Board),Bewertung) :- % gegner kann gewinnen --> zu muss schlecht bewertet werden
	swap_player(P,Enemy),
	select_win([D],state(player(Enemy),Board),_),
	%write("enemy_node >>> Tiefe: "), write(Tiefe),write(", Column: "),write(D),write(" -- enemy can win -- "),write(state(player(P),Board)),nl,
	enemy_node(Tiefe,Ds,state(player(P),Board),WertRest),
	min(-1000,WertRest,Bewertung), 
	!.
enemy_node(Tiefe,[D|Ds],state(player(P),Board),Bewertung) :- % gegner kann nicht gewinnen --> bekommt wert von kinder nodes <-- own_node (Wert)
	swap_player(P,Enemy),
	\+ select_win([D],state(player(Enemy),Board),_),
	%write("enemy_node >>> Tiefe: "), write(Tiefe),write(", Column: "),write(D),write(" -- enemy cannot win -- "),write(state(player(P),Board)),nl,
	move(state(player(Enemy),Board),D,state(_,board([L|Ls],W))),
	DecTiefe is Tiefe-1, 
	domain(L,0,NewDomain),
	own_node(DecTiefe,NewDomain,state(player(P),board([L|Ls],W)),Wert),
	enemy_node(Tiefe,Ds,state(player(P),Board),WertRest),
	min(Wert,WertRest,Bewertung),
	!.
	
	
% gibt einen zufälligen gültigen zug zurück
random_logic(game(state(_,board([L|_],_)),_,_,_),SelectedColumn) :- domain(L,0,Dom), random_member(SelectedColumn,Dom).

% verhindert das gewinnen des gegners durch level1 forecasting
% versucht selbst nicht zu gewinnen!
logic_02(game(state(player(P),board([L|Ls],W)),_,_,_),SelectedColumn) :- 
	domain(L,0,Domain),
	funclvl1(Domain, state(player(P),board([L|Ls],W)), NewDomain),
	random_member(SelectedColumn,NewDomain).

% KI kann gewinnen! (s. select_win/3)
logic_03(game(state(player(P),board([L|Ls],W)),_,_,_),SelectedColumn) :- 
	domain(L,0,Domain),
	select_win(Domain,state(player(P),board([L|Ls],W)),SelectedColumn).

% gegner wird auf jeden fall gewinnen
% SaveDomain ist leer, es gibt keinen zug der den sieg des gegners verhindern könnte
% also setzt er random irgendwo hin
logic_03(game(state(player(P),board([L|Ls],W)),_,_,_),SelectedColumn) :- 
	domain(L,0,Domain),
	\+ select_win(Domain,state(player(P),board([L|Ls],W)),_),
	exclude_enemys_win(Domain, state(player(P),board([L|Ls],W)), SaveDomain),% nl, write(BetterDomain),
	SaveDomain = [],
	random_member(SelectedColumn,Domain).
	
	
% rechnet beide bewertungsListen zusammen und nimmt den höchsten
% in SaveDomain sind die züge nicht mehr drin mit denen man verlieren würde
% über den index in der BewList wird die Spalte in Domain gesucht, die zurückgegeben wird
logic_03(game(state(player(P),board([L|Ls],W)),_,_,_),SelectedColumn) :- 
	domain(L,0,Domain),
	\+ select_win(Domain,state(player(P),board([L|Ls],W)),_),
	exclude_enemys_win(Domain, state(player(P),board([L|Ls],W)), SaveDomain),% nl, write("deny user win: "), write(BetterDomain),
	\+ SaveDomain = [],
	bewertung_1(SaveDomain,state(player(P),board([L|Ls],W)),ErgBew1),% nl, write(ErgBew1),
	bewertung_2(SaveDomain,state(player(P),board([L|Ls],W)),ErgBew2),% nl, write(ErgBew2),
	add_lists_together(ErgBew1,ErgBew2,BewList),% nl, write(BewList),
	hightest_member(BewList,Hightest),% nl, write("hightest: "), write(Hightest),
	indexOf(BewList,Hightest,IndexVList),
	indexOf(SaveDomain,SelectedColumn,IndexVList). 
	
% gibt eine bewertung zu jeder column (Ds) in der Liste Erg aus 
% // bewertung_1 kümmert sich um das füllen eigener listen
% // in der bewerungsliste sind int die sich aus dem runden (s.u.) ableiten,
% // es wird das verhältniss der anzahl der gesetzten steine in den WLists und lände der WLists vor einem möglichen setzten 
% // und nach dem setzten verglichen, wenn das verhältniss sich erhöht, wurde auch min. eine reihe vervollständigt!!
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
	E is round((float(Count2/Len2not0)-float(Count1/Len1not0))*100), % faktor gibt die wertung gegenüber bewertung_2 /100 an!
	bewertung_1(Ds,state(player(P),Board),Erest), !.
	
% gibt eine bewertung zu jeder column (Ds) in der Liste Erg aus 
% // bewertung_2 kümmert sich um das BLOCKEN DES GEGNERS!
% // für jede geblocke WList des gegners wird inkrementiert 
% // letztlich wird aus float-rundungsgründen in bewertung_1 um 100 multipliziert
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
% wenn gewonnen werden kann wir D zurück gegeben sonst false
select_win([D|_], CurrentState, D) :-
	move(CurrentState,D,NewState),
	win_board(NewState). 

select_win([D|Ds], CurrentState, E) :-
	move(CurrentState,D,NewState),
	\+ win_board(NewState),
	select_win(Ds, CurrentState, E).
	
% gibt nur domains zurück, wenn der gegner danach nicht gewinnen kann! (einen zug vorraus berechnen)
% füg jedes D nur dann der liste zu wenn der gegner nicht nach einem zug in D von der KI gewinnen kann.
% wenn die KI nicht mehr gewinnen kann, ist die liste leer.
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

% gibt den kleinesten member von iter1 zurück --> die spalte in die gesetzt wird
% das ist der zug in dem die KI am meisten mögliche gewinnreihen verhindern kann.
logic_01(game(state(player(P),board([L|Ls],W)),_,_,_),SelectedColumn) :- 
	domain(L,0,Domain),
	swap_player(P,Enemy),% write(Dimension),
	iter1(Domain, P, Enemy, state(player(P),board([L|Ls],W)), ValueList), nl, write(ValueList),
	lowest_member(ValueList,Lowest),
	indexOf(ValueList,Lowest,IndexVList),
	indexOf(Domain,SelectedColumn,IndexVList), !. 
	
% iteriert durch die liste domain durch..  gibt für jede iteration ein int in die erg liste ein,
% welcher die anzahl der möglichen gewinn reihen des gegners zurück gibt
iter1([], _, _, _, []). 
iter1([D|Ds], P, Enemy, CurrentState, Erg) :- 
	Erg = [N|Erest],
	move(CurrentState, D, state(player(_), NewBoard)),
	all_notPlayer_WLists(NewBoard,player(P),WLists), nl, write(D), write(" : "), write(WLists),
	count_P(WLists,player(Enemy),N),%sst  <-- wertung 
	iter1(Ds, P, Enemy, CurrentState, Erest), !.
	
% wendet count_empty_in_list auf eine liste von listen an und quadriert die 
% einzelnnen ergebnisse aus jeder liste bevor sie summiert werden
count_empty([],player(_),0).
count_empty([L|Ls],player(P),Erg) :- 
	count_empty_in_list(L,player(P),CountOfList), 
	count_empty(Ls,player(P),Erest), 
	Erg is Erest + (CountOfList*CountOfList), !.

% counts the ' ' (empty elements) of a list 
% (),player,Int
count_empty_in_list([],player(_),0).
count_empty_in_list([L|Ls],player(P),Erg) :- empty(L), count_empty_in_list(Ls,player(P),Erest), (Erg is Erest +1), !.
count_empty_in_list([L|Ls],player(P),Erg) :- \+ empty(L), count_empty_in_list(Ls,player(P),Erg), !.

% eine hilfsfunktion um das teilen durch 0 zu verhindern // macht aus 0 eine 1 sonst bleibt N = N
zero_to_one(N,N) :- N \= 0.
zero_to_one(N,E) :- N = 0, E=1.