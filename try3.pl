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


relationship(F, father, C) :- father(F, C).
relationship(M, mother, C) :- mother(M, C).
relationship(GF, grandfather, GC) :- grandfather(GF, GC).
relationship(GM, grandmother, GC) :- grandmother(GM, GC).
relationship(B, brother, S) :- brother(B, S).
relationship(Sis, sister, S) :- sister(Sis, S).
relationship(U, uncle, C) :- uncle(U, C).
relationship(A, aunt, C) :- aunt(A, C).
relationship(N, nephew, P) :- nephew(N, P).
relationship(Nc, niece, P) :- niece(Nc, P).
relationship(C, cousin, P) :- cousin(C, P).
relationship(h, spouse, w) :- spouse(h, w).


% Helper predicate to find the shortest path in terms of relationships
shortest_path(Person1, Person2, Path) :-
    % Use breadth-first search to find the shortest path
    breadth_first([[Person1]], Person2, RevPath),
    % Reverse the path to get the correct order from Person1 to Person2
    reverse(RevPath, Path).

% Breadth-first search implementation
breadth_first([[Person2 | Path] | _], Person2, [Person2 | Path]).
breadth_first([[Person1 | Path] | Paths], Person2, Solution) :-
    extend_path(Person1, Path, NewPaths),
    append(Paths, NewPaths, Paths1),
    breadth_first(Paths1, Person2, Solution).

% Extend a path to all directly related persons not yet in the path
extend_path(Person, Path, NewPaths) :-
    findall([NewPerson, Person | Path],
            (relationship(Person, _, NewPerson), \+ member(NewPerson, Path)),
            NewPaths).

% Show the shortest relationship path between two persons
show_shortest_relationship(Person1, Person2) :-
    (   shortest_path(Person1, Person2, Path) ->
        write('The shortest relationship path from '), 
        write(Person1), 
        write(' to '), 
        write(Person2), 
        write(' is: '), 
        print_path(Path)
    ;   shortest_path(Person2, Person1, Path) ->
        write('The shortest relationship path from '), 
        write(Person2), 
        write(' to '), 
        write(Person1), 
        write(' is: '), 
        print_path(Path)
    ;   write('No relationship path found between '), 
        write(Person1), 
        write(' and '), 
        write(Person2), 
        write('.\n')
    ).

% Helper predicate to print the path of relationships
print_path([]) :-
    nl.
print_path([Person]) :-
    write(Person), nl.
print_path([Person1, Person2 | Rest]) :-
    relationship(Person1, Relation, Person2),
    format('~w (~w) -> ', [Person1, Relation]),
    print_path([Person2 | Rest]).



