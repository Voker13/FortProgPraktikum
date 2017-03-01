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
		direct_print/1,
		indexOf/3
	]).
	
:- use_module(game_logic, [
		random_logic/2,
		logic_01/2, 
		logic_02/2,
		logic_03/2 as logic
	]). 

start() :- 	write("How many Rows the field should have:"),read(Rows),
			write("How many Columns the field should have:"),read(Cols),
			max(Rows,Cols,Max),
			write("Streak to Win between 1 - "), write(Max), write("!"), read(W),
			write("Choose 'x' or 'o' ('x' starts):"),read(Human),
			new_empty_board(Cols,Rows,NewBoard), 
			turn(game(state(player(x),board(NewBoard,W)),Human,Rows,Cols)).
			
% no one has won so far...
evaluation(game(state(player(P),NewBoard),Human,Rows,Cols)) :- 	\+ win_board(state(player(P),NewBoard)), 
																\+ draw_board(NewBoard), 
																swap_player(P,NewP),
																turn(game(state(player(NewP),NewBoard),Human,Rows,Cols)).
																
% human has won! 
evaluation(game(state(player(P),NewBoard),Human,_,_)) :- 	win_board(state(player(P),NewBoard)),
																player(P) = player(Human),
																write("Human wins!"),nl,
																show_board(NewBoard).
																
% KI has won! 
evaluation(game(state(player(P),NewBoard),Human,_,_)) :- 	win_board(state(player(P),NewBoard)),
																player(P) \= player(Human),
																write("KI wins!"),nl,
																show_board(NewBoard).
																
% DRAW! 
evaluation(game(state(player(_),NewBoard),_,_,_)) :- 	draw_board(NewBoard),
																write("Draw!"),nl,
																show_board(NewBoard).

% humans turn - not won
turn(game(state(player(P),board([L|Ls],W)),Human,Rows,Cols)) :-
						player(P) = player(Human),
						domain(L,0,Dom),
						write("It's your turn - choose wisely!"),nl,write(Dom),nl,%write(")!"),nl,
						show_board(board([L|Ls],W)),
						read(Column),
						integer(Column),
						indexOf(Dom,Column,_), 
						move(state(player(P),board([L|Ls],W)),Column,state(player(P),NewBoard)),																		
						evaluation(game(state(player(P),NewBoard),Human,Rows,Cols)).
	
% KI turn - not won										 								
turn(game(state(player(P),board([L|Ls],W)),Human,Rows,Cols)) :-	
						player(P) \= player(Human),
						write("It's KIs turn!"),nl,	
						show_board(board([L|Ls],W)),
						direct_print("KI is thinking"), 
						logic(game(state(player(P),board([L|Ls],W)),Human,Rows,Cols),SelectedCol),  
						write("The KI set in Column "), write(SelectedCol), write("."),nl,
						move(state(player(P),board([L|Ls],W)),SelectedCol,state(player(P),NewBoard)),																	
						evaluation(game(state(player(P),NewBoard),Human,Rows,Cols)).
						
