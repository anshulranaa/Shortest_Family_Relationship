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

cousin(Cousin, Person) :-
    (
        parent(Parent, Person),
        sibling(Parent, Uncle),
        male(Uncle),
        parent(Uncle, Cousin)
    ;
        parent(Parent, Person),
        sibling(Parent, Aunt),
        female(Aunt),
        parent(Aunt, Cousin)
    ).


% Define relationships
relationship(father, Father, Child) :- father(Father, Child).
relationship(mother, Mother, Child) :- mother(Mother, Child).
relationship(grandfather, Grandfather, Grandchild) :- grandfather(Grandfather, Grandchild).
relationship(grandmother, Grandmother, Grandchild) :- grandmother(Grandmother, Grandchild).
relationship(uncle, Uncle, Child) :- uncle(Uncle, Child).
relationship(aunt, Aunt, Child) :- aunt(Aunt, Child).
relationship(nephew, Nephew, Person) :- nephew(Nephew, Person).
relationship(niece, Niece, Person) :- niece(Niece, Person).
relationship(cousin, Cousin, Person) :- cousin(Cousin, Person).

format_path([], '').

% This clause handles a single relationship in the path.
format_path([rel(Rel, A, B)], String) :-
    format(atom(String), '~w(~w, ~w)', [Rel, A, B]).

% This clause handles multiple relationships in the path.
format_path([rel(Rel, A, B) | Rest], String) :-
    format_path(Rest, RestString),
    format(atom(String), '~w(~w, ~w) > ~s', [Rel, A, B, RestString]).

    

% Utility to find and show paths
find_path(Start, End, Path) :-
    find_path(Start, End, [], RevPath),
    reverse(RevPath, Path).

find_path(Person, Other, Acc, [rel(Relation, Person, Other) | Acc]) :-
    relationship(Relation, Person, Other).

% Recursively find a relationship path, ensuring to wrap each step in the 'rel' functor
find_path(Person, Other, Acc, Path) :-
    relationship(Relation, Person, Intermediate),
    Intermediate \= Other,
    \+ memberchk(rel(_, Intermediate, _), Acc),  % Enhanced check: Avoid any past intermediate
    find_path(Intermediate, Other, [rel(Relation, Person, Intermediate) | Acc], Path).

show_relationship(P1, P2) :-
    find_path(P1, P2, Path),
    format_path(Path, String),
    format('~w is related to ~w via: ~s\n', [P1, P2, String]).

show_relationship(P1, P2) :-
    \+ find_path(P1, P2, _),
    writeln('No known relationship.').

