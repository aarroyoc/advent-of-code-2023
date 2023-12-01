:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(charsio)).
:- use_module(library(reif)).
:- use_module(library(dif)).
:- use_module(library(lists)).

lines([]) --> [].
lines([X|Xs]) -->
    line(X),
    lines(Xs).

line([]) --> "\n".
line([X|Xs]) -->
    [X],
    line(Xs).

digits(Line, DigitA, DigitB) :-
    tfilter(digit_t, Line, Digits),
    Digits = [DigitA|_],
    reverse(Digits, [DigitB|_]).

digit_t(Digit, true) :-
    char_type(Digit, decimal_digit).

digit_t(Digit, false) :-
    char_type(Digit, Type),
    dif(Type, decimal_digit).

line_value(Line, Value) :-
    digits(Line, DigitA, DigitB),
    number_chars(Value, [DigitA, DigitB]).

main(Sum) :-
    phrase_from_file(lines(Lines), "input"),
    maplist(line_value, Lines, Values),
    sum_list(Values, Sum).

text_digit("1") --> "one".
text_digit("2") --> "two".
text_digit("3") --> "three".
text_digit("4") --> "four".
text_digit("5") --> "five".
text_digit("6") --> "six".
text_digit("7") --> "seven".
text_digit("8") --> "eight".
text_digit("9") --> "nine".

text_digit_reverse("1") --> "eno".
text_digit_reverse("2") --> "owt".
text_digit_reverse("3") --> "eerht".
text_digit_reverse("4") --> "ruof".
text_digit_reverse("5") --> "evif".
text_digit_reverse("6") --> "xis".
text_digit_reverse("7") --> "neves".
text_digit_reverse("8") --> "thgie".
text_digit_reverse("9") --> "enin".

main2(Sum) :-
    phrase_from_file(lines(Lines), "input"),
    maplist(line_text_digits, Lines, LinesTextDigits),
    maplist(line_value, LinesTextDigits, Values),
    sum_list(Values, Sum).

line_text_digits(Line, LineTextDigitsFull) :-
    phrase(line_text_digits_(LineTextDigits), Line),
    reverse(Line, LineReverse),
    phrase(line_text_digits_reverse_(LineTextDigitsReverse),LineReverse),
    reverse(LineTextDigitsReverse, LTDR1),
    append(LineTextDigits, LTDR1, LineTextDigitsFull).

line_text_digits_([]) --> [].
line_text_digits_(Line) -->
    text_digit(X),
    seq(Xs),
    { append(X, Xs, Line) }.
line_text_digits_(Line) -->
    [X],
    line_text_digits_(Xs),
    { append([X], Xs, Line) }.

line_text_digits_reverse_([]) --> [].
line_text_digits_reverse_(Line) -->
    text_digit_reverse(X),
    seq(Xs),
    { append(X, Xs, Line) }.
line_text_digits_reverse_(Line) -->
    [X],
    line_text_digits_reverse_(Xs),
    { append([X], Xs, Line) }.
