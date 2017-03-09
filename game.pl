:- use_module(game_methods, [
		empty/1,
		player/1,
		draw_board/1,
		win_board/1,
		swap_player/2,
		show_board/1,
		domain/3,
		move/3,
		max/3,
		new_empty_board/3,
		direct_print_nl/1,
		indexOf/3
	]).
	
:- use_module(library(lists)).

:- use_module(game_logic, [
		random_logic/2,
		logic_01/2, 
		logic_02/2,
		logic_03/2,
		logic_tree/2 as logic
	]). 
  
  
% startet das Spiel in der Konsole, Benutzereingaben bestimmen die Dimension des Spielfelds,
% die Anzahl der Steine in einer Reihe f�r den Gewinn und das Symbol des Spielers
% Spieler x beginnt 
start() :- 	write("How many Rows the field should have:"), read_int_pos(Rows),% read(Rows), integer(Rows),
			write("How many Columns the field should have:"),read_int_pos(Cols), 
			max(Rows,Cols,Max),
			write("Streak to Win between 1 - "), write(Max), write("!"), read_int_pos_max(Max,W),
			write("Choose 'x' or 'o' ('x' starts):"),read_human(Human),
			new_empty_board(Cols,Rows,NewBoard), 
			turn(game(state(player(x),board(NewBoard,W)),Human,Rows,Cols)). 
			
% die folgenden 4 terms prüfen die user-eingaben auf richtige inputs
read_int_pos(X) :- read(X), integer(X), X > 0, !; write("Wong input! You have to enter a Integer > 0!"), read_int_pos(X).
read_int_pos_max(Max,X) :- read(X), integer(X), X > 0, X =< Max, !; write("Wong input! You have to enter a Integer > 0 and <= "), write(Max), write("!"), read_int_pos_max(Max,X).
read_human(X) :- read(X), player(X), !; write("Wrong Input! Enter 'x' or 'o'!"), read_human(X).
read_int_domain(X,Domain) :- read(X), member(X,Domain), !; write("Wrong input! Choose on of these: "), write(Domain), read_int_domain(X,Domain).

% der Zug des Spielers:
% es wird das aktuelle Board ausgegeben und der Spieler wird aufgefordert, einen
% Stein in eine Spalte zu werfen, die Eingabe wird �berpr�ft und dann wird erst
% der Zug mit move durchgef�hrt, anschlie�end wird der Zustand des Boards auf Gewinn
% oder Unentschieden gecheckt
turn(game(state(player(P),board([L|Ls],W)),Human,Rows,Cols)) :-
						player(P) = player(Human),
						domain(L,0,Domain),
						write("It's your turn - choose wisely!"),nl,write(Domain),nl,%write(")!"),nl,
						show_board(board([L|Ls],W)), 
						read_int_domain(Column,Domain),
						%integer(Column),
						%indexOf(Dom,Column,_), 
						move(state(player(P),board([L|Ls],W)),Column,state(player(P),NewBoard)),																		
						evaluation(game(state(player(P),NewBoard),Human,Rows,Cols)).
	
% Zug der KI:
% der einzige Unterschied zum Spielerzug ist, dass die KI mit der in game_logic implementierten
% Logik eine optimale Spalte ausw�hlt, danach wird damit genauso der Zug durchgef�hrt
% und das Board evaluiert					 								
turn(game(state(player(P),board([L|Ls],W)),Human,Rows,Cols)) :-	 
						player(P) \= player(Human),
						write("It's KIs turn!"),nl,	
						show_board(board([L|Ls],W)),  
						direct_print_nl("KI is thinking..."), 
						logic(game(state(player(P),board([L|Ls],W)),Human,Rows,Cols),SelectedCol),  
						write("The KI set in Column "), write(SelectedCol), write("."),nl,
						move(state(player(P),board([L|Ls],W)),SelectedCol,state(player(P),NewBoard)),																	
						evaluation(game(state(player(P),NewBoard),Human,Rows,Cols)).




% bei der Evaluation wird �berpr�ft, ob eine Gewinnsituation vorliegt oder ob es sich um ein Unentschieden handelt
% und wird entsprechend ausgegeben

% keiner hat gewonnen und es liegt kein Unentschieden vor, das Spiel geht weiter, der Spieler wird gewechselt
evaluation(game(state(player(P),NewBoard),Human,Rows,Cols)) :- 	\+ win_board(state(player(P),NewBoard)), 
																\+ draw_board(NewBoard), 
																swap_player(P,NewP),
																turn(game(state(player(NewP),NewBoard),Human,Rows,Cols)).
																
% der Spieler hat gewonnen
evaluation(game(state(player(P),NewBoard),Human,_,_)) :- 	win_board(state(player(P),NewBoard)),
																player(P) = player(Human),
																write("Human wins!"),nl,
																show_board(NewBoard).
																
% die KI hat gewonnen
evaluation(game(state(player(P),NewBoard),Human,_,_)) :- 	win_board(state(player(P),NewBoard)),
																player(P) \= player(Human),
																write("KI wins!"),nl,
																show_board(NewBoard).
																
% es liegt ein Unentschieden vor
evaluation(game(state(player(_),NewBoard),_,_,_)) :- 	draw_board(NewBoard),
																write("Draw!"),nl,
																show_board(NewBoard).

