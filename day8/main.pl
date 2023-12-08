:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(pio)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(assoc)).
:- use_module(library(reif)).
:- use_module(library(dif)).
:- use_module(library(iso_ext)).

input(Instructions, Map) -->
    seq(Instructions),nl,
    nl,
    input_map(Map).

nl --> "\n".

input_map(X) --> [], { empty_assoc(X) }.
input_map(X) -->
    seq(Name), " = (", seq(Left), ", ", seq(Right), ")",nl,
    input_map(X0),
    {
	put_assoc(Name, X0, node(Left, Right), X)
    }.

main(X) :-
    phrase_from_file(input(Instructions, Map), "input"),!,
    StartNode = "AAA",
    route(Instructions, Instructions, StartNode, Map, 0, X).

% TODO - Too slow
main2(X) :-
    phrase_from_file(input(Instructions, Map), "input"),!,
    $ starting_nodes(Map, StartingNodes),
    route2([], Instructions, StartingNodes, Map, 0, X).

route2(_, _, CurrentNodes, _, C, C) :-
    forall(member(Node, CurrentNodes), append(_, "Z", Node)).

route2([], Is, CurrentNodes, Map, C0, C) :-
    route2(Is, Is, CurrentNodes, Map, C0, C).

route2([X|Xs], Is, CurrentNodes, Map, C0, C) :-
    maplist(node_forward(X, Map), CurrentNodes, Nodes),
    C1 #= C0 + 1,
    route2(Xs, Is, Nodes, Map, C1, C).

node_forward('L', Map, Node0, Node) :-
    get_assoc(Node0, Map, node(Node, _)).

node_forward('R', Map, Node0, Node) :-
    get_assoc(Node0, Map, node(_, Node)).

starting_nodes(Map, StartingNodes) :-
    assoc_to_keys(Map, Keys),
    tfilter(starting_node_t, Keys, StartingNodes).

starting_node_t(Node, true) :-
    append(_, "A", Node).

starting_node_t(Node, false) :-
    append(_, [C], Node),
    dif(C, 'A').

route(_, _, "ZZZ", _, C, C).

route([], Is, CurrentNode, Map, C0, C) :-
    route(Is, Is, CurrentNode, Map, C0, C).

route(['L'|Xs], Is, CurrentNode, Map, C0, C) :-
    get_assoc(CurrentNode, Map, node(Left, _)),
    C1 #= C0 + 1,
    route(Xs, Is, Left, Map, C1, C).

route(['R'|Xs], Is, CurrentNode, Map, C0, C) :-
    get_assoc(CurrentNode, Map, node(_, Right)),
    C1 #= C0 + 1,
    route(Xs, Is, Right, Map, C1, C).
