:- module(game_logic,[
		random_logic/2
	]).

:- use_module(game_methods, [
		
	]).
	
random_logic(Dim,Column) :- random_member(Column,Dim).

