% Define gender
male(lockie).
female(mary).
% Assuming gender based on names, adjust as necessary
female(milli).
male(jhonty).
female(ria).
male(david).
female(sushi).
female(lucy).
male(roy).
female(dolly).
female(dia).
male(kevin).
female(katie).

% Define spousal relationships
spouse(lockie, mary).
spouse(mary, lockie).
spouse(milli,jhonty).
spouse(jhonty,milli).
spouse(ria,david).
spouse(david,ria).
spouse(lucy,roy).
spouse(roy,lucy).
spouse(katie,kevin).
spouse(kevin,katie).

% Define parental relationships
parent(lockie, jhonty).
parent(mary, jhonty).
parent(lockie, david).
parent(mary, david).
parent(lockie, sushi).
parent(mary, sushi).

% Assuming who the parents of Roy, Dia, Kevin, and Katie are, based on the tree structure
parent(milli, roy).
parent(jhonty, roy).
parent(jhonty, dolly).
parent(milli, dolly).
parent(ria, dia).
parent(david, dia).
parent(roy, katie).
parent(lucy, katie).
parent(roy, kevin).
parent(lucy, kevin).

% Define parent as a generalization of mother and father
father(Father, Child) :-
    parent(Father, Child),
    male(Father).

% Infer the motherhood if the female spouses husband is the father of the child
mother(Mother, Child) :-
    parent(Mother, Child),
    female(Mother).

grandfather(GF, GC) :-
    parent(P, GC),
    father(GF, P).
grandmother(GM, GC) :- 
    parent(P, GC),
    mother(GM, P).

sibling(Person1, Person2) :-
    parent(Parent, Person1),
    parent(Parent, Person2),
    Person1 \= Person2.

% Brother relationship
brother(Person, Sibling) :-
    sibling(Person, Sibling),
    male(Person).
    

% Sister relationship
sister(Person, Sibling) :-
    female(Person),
    sibling(Person, Sibling).

uncle(Uncle, Child) :-
    male(Uncle),
    (   
        father(Father, Child),
        brother(Uncle, Father)
    ;   
        mother(Mother, Child),
        brother(Uncle, Mother)
    ).

aunt(Aunt, Child) :-
    female(Aunt),
    (   
        father(Father, Child),
        sister(Aunt, Father)
    ;   
        mother(Aunt, Child),
        sister(Aunt, Mother)
    ).

nephew(Nephew, Person) :-
    male(Nephew),
    parent(Parent, Nephew),
    sibling(Parent, Person).

niece(Niece, Person) :-
    female(Niece),
    parent(Parent, Niece),
    sibling(Parent, Person).


% Redefine relationships with a two-argument structure for direct queries

relationship(F, C) :- father(F, C), format('~w is a father of ~w.', [F, C]).
relationship(M, C) :- mother(M, C), format('~w is a mother of ~w.', [M, C]).
relationship(GP, GC) :- grandfather(GP, GC), format('~w is a grandfather of ~w.', [GP, GC]).
relationship(GC, GP) :- grandmother(GC, GP), format('~w is a grandmother of ~w.', [GC, GP]).
relationship(U, C) :- uncle(U, C), format('~w is an uncle of ~w.', [U, C]).
relationship(A, C) :- aunt(A, C), format('~w is an aunt of ~w.', [A, C]).
relationship(N, X) :- nephew(N, X), format('~w is a nephew of ~w.', [N, X]).
relationship(N, X) :- niece(N, X), format('~w is a niece of ~w.', [N, X]).

% Helper predicate to display relationships
show_relationship(P1, P2) :-
    relationship(P1, P2), !; % Find a relationship and stop.
    writeln('No known relationship.').
