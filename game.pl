:- use_module(game_methods, [
		empty/1,
		player/1,
		draw_board/1,
		win_board/2,
		swap_player/2,
		show_board/1,
		dimension/3,
		move/3,
		new_empty_board/3
	]).
	
:- use_module(game_logic, [
		random_logic/2 as logic
	]).

start() :- 	write("How many Rows the field should have:"),read(Rows),
			write("How many Columns the field should have:"),read(Cols),
			write("Streak to Win:"),read(W),
			write("Choose 'x' or 'o' ('x' starts):"),read(Human),
			new_empty_board(Cols,Rows,NewBoard),
			turn(game(state(player(x),board(NewBoard,W)),Human,Rows,Cols)).
			
% no one has won so far...
evaluation(game(state(player(P),NewBoard),Human,Rows,Cols)) :- 	\+ win_board(player(P),NewBoard), 
																\+ draw_board(NewBoard), 
																swap_player(P,NewP),
																turn(game(state(player(NewP),NewBoard),Human,Rows,Cols)).
																
% human has won! 
evaluation(game(state(player(P),NewBoard),Human,_,_)) :- 	win_board(player(P),NewBoard),
																player(P) = player(Human),
																write("Human wins!"),nl,
																show_board(NewBoard).
																
% KI has won! 
evaluation(game(state(player(P),NewBoard),Human,_,_)) :- 	win_board(player(P),NewBoard),
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
						dimension(L,0,Dim),
						write("It's your turn - choose wisely between "),write(Dim),write(")!"),nl,
						show_board(board([L|Ls],W)),
						read(Column),
						move(state(player(P),board([L|Ls],W)),Column,state(player(P),NewBoard)),																		
						evaluation(game(state(player(P),NewBoard),Human,Rows,Cols)).
	
% KI turn - not won																			
turn(game(state(player(P),board([L|Ls],W)),Human,Rows,Cols)) :-	
						player(P) \= player(Human),
						write("It's KIs turn!"),nl,	
						show_board(board([L|Ls],W)),
						dimension(L,0,Dim),
						logic(Dim,Column),  
						write("The KI set in Column "), write(Column), write("."),nl,
						move(state(player(P),board([L|Ls],W)),Column,state(player(P),NewBoard)),																	
						evaluation(game(state(player(P),NewBoard),Human,Rows,Cols)).