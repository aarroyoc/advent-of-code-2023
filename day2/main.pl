:- use_module(library(reif)).
:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(library(dcgs)).
:- use_module(library(pio)).
:- use_module(library(iso_ext)).
:- use_module(library(format)).

lines([]) --> [].
lines([X|Xs]) -->
    line(X),
    lines(Xs).

line([]) --> "\n".
line([X|Xs]) -->
    [X],
    line(Xs).

game(game(Id, Plays)) -->
    "Game ",
    seq(IdCs),
    ":",
    { number_chars(Id, IdCs) },
    plays(Plays).

plays([X]) --> play(X).
plays([X|Xs]) -->
    play(X),
    ";",
    plays(Xs).

play(play(Red, Green, Blue)) -->
    " ", seq(Red), " red",
    { \+ member(',', Red), \+ member(';', Red) },
    ( [] | (",", play(play(Red, Green, Blue)))).
play(play(Red, Green, Blue)) -->
    " ", seq(Green), " green",
    { \+ member(',', Green), \+ member(';', Green) },
    ( [] | (",", play(play(Red, Green, Blue)))). 
play(play(Red, Green, Blue)) -->
    " ", seq(Blue), " blue",
    { \+ member(',', Blue), \+ member(';', Blue) },
    ( [] | (",", play(play(Red, Green, Blue)))). 

main(X) :-
    phrase_from_file(lines(Lines), "input"),
    maplist(lines_games, Lines, Games),
    tfilter(elf_condition, Games, ValidGames),
    sum_game_ids(ValidGames, X).

main2(X) :-
    phrase_from_file(lines(Lines), "input"),
    maplist(lines_games, Lines, Games),
    maplist(games_power, Games, Power),
    sum_list(Power, X).

games_power(game(_, Plays), Power) :-
    games_power_(Plays, Red, Green, Blue),
    Power #= Red * Green * Blue.

games_power_([], 0, 0, 0).
games_power_([play(Red0, Green0, Blue0)|Xs], Red, Green, Blue) :-
    Red #= max(Red0, Red1),
    Green #= max(Green0, Green1),
    Blue #= max(Blue0, Blue1),
    games_power_(Xs, Red1, Green1, Blue1).

sum_game_ids([], 0).
sum_game_ids([game(Id, _)|Xs], Sum) :-
    Sum #= Id + Sum0,
    sum_game_ids(Xs, Sum0).

elf_condition(game(Id, Plays), true) :-
    forall(member(Play, Plays), (
               Play = play(Red, Green, Blue),
	       Red #< 13,
	       Green #< 14,
	       Blue #< 15
	   )).

elf_condition(game(Id, Plays), false) :-
    \+ forall(member(Play, Plays), (
               Play = play(Red, Green, Blue),
	       Red #< 13,
	       Green #< 14,
	       Blue #< 15
	   )).

lines_games(Line, Game) :-
    phrase(game(Game0), Line),
    portray_clause(Game0),
    normalize(Game0, Game),
    portray_clause(Game).

normalize(game(Id, Plays0), game(Id, Plays1)) :-
    normalize_(Plays0, Plays1).

normalize_([], []).
normalize_([play(Red0, Green0, Blue0)|Xs0], [play(Red, Green, Blue)|Xs]) :-
    (
	var(Red0) ->
	Red = 0
    ;   number_chars(Red, Red0)
    ),
    (
	var(Green0) ->
	Green = 0
    ;   number_chars(Green, Green0)
    ),
    (
	var(Blue0) ->
	Blue = 0
    ;   number_chars(Blue, Blue0)
    ),
    normalize_(Xs0, Xs).
