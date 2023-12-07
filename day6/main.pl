:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(iso_ext)).

input(Times, Distances) -->
    "Time:", spaces, spaced_nums(Times),
    "Distance:", spaces, spaced_nums(Distances).

input2(Time, Distance) -->
    "Time:", spaces, num(TimeCs),
    "Distance:", spaces, num(DistanceCs),
    { number_chars(Time, TimeCs), number_chars(Distance, DistanceCs) }.

num("") --> nl.
num(Num) -->
    " ",
    num(Num).
num([X|Num0]) -->
    [X],
    num(Num0).


nl --> "\n".

spaces --> " ", spaces.
spaces --> " ".

spaced_nums([]) --> nl.
spaced_nums([X|Xs]) -->
    spaces, seq(NumCs),
    spaced_nums(Xs),
    { number_chars(X, NumCs) }.

main(X) :-
    phrase_from_file(input(Times, Distances), "input"),
    maplist(race_count, Times, Distances, Vals),
    mul_list(Vals, X).

main2(X) :-
    phrase_from_file(input2(Time, Distance), "input"),
    race_count2(Time, Distance, X).

mul_list([], 1).
mul_list([X|Xs], N) :-
    N #= X*N0,
    mul_list(Xs, N0).

race_count2(Time, Distance, X) :-
    race_count_up(Time, Distance, X0),
    race_count_down(Time, Distance, X1),
    X #= X1 - X0 + 1.

race_count_up(Time, Distance, TimeForAccel) :-
    [TimeForAccel, TimeForRun] ins 0..Time,
    Time #= TimeForAccel + TimeForRun,
    Speed #= TimeForAccel,
    DistanceRun #= Speed * TimeForRun,
    DistanceRun #> Distance,
    labeling([up], [TimeForAccel]).

race_count_down(Time, Distance, TimeForAccel) :-
    [TimeForAccel, TimeForRun] ins 0..Time,
    Time #= TimeForAccel + TimeForRun,
    Speed #= TimeForAccel,
    DistanceRun #= Speed * TimeForRun,
    DistanceRun #> Distance,
    labeling([down], [TimeForAccel]).
    
race_count(Time, Distance, N) :-
    countall(race(Time, Distance, _), N).

race(Time, Distance, TimeForAccel) :-
    [TimeForAccel, TimeForRun] ins 0..Time,
    Time #= TimeForAccel + TimeForRun,
    Speed #= TimeForAccel,
    DistanceRun #= Speed * TimeForRun,
    DistanceRun #> Distance,
    label([TimeForAccel, TimeForRun]).
