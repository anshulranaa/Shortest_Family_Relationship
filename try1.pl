% Define gender
female(alice).
female(eve).
female(kate).
male(bob).
male(carlos).
male(dave).

% Define parental relationships
mother(kate, alice).
mother(alice, bob).
father(carlos, bob).

% Define child relationships based on parental relationships
child(C, P) :- mother(P, C).
child(C, P) :- father(P, C).

% Define parent as a generalization of mother and father
parent(X, Y) :- mother(X, Y).
parent(X, Y) :- father(X, Y).

% Define sibling relationships
sister(alice, eve).
sister(eve, alice).
sister(alice, dave).
brother(dave, alice).

% Generalize sibling relationships
sibling(X, Y) :- brother(X, Y).
sibling(X, Y) :- sister(X, Y).

% Define uncle relationship based on sibling and child relationships
uncle(U, C) :- 
    sibling(U, Parent), 
    child(C, Parent), 
    male(U).

% Redefine various relationships with a two-argument structure
relationship(P, C) :- parent(P, C), format('~w is a parent of ~w', [P, C]).
relationship(U, C) :- uncle(U, C), format('~w is an uncle of ~w', [U, C]).
relationship(S, O) :- sibling(S, O), format('~w is a sibling of ~w', [S, O]).
relationship(G, C) :- parent(P, C), parent(G, P), format('~w is a grandparent of ~w', [G, C]).

% Helper predicate to display relationships (optional)
show_relationship(P1, P2) :-
    relationship(P1, P2), !; % Find a relationship and stop.
    relationship(P2, P1), !; % Reverse the order if no direct relationship is found.
    writeln('No known relationship.').

