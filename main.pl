:- dynamic male/1.
:- dynamic female/1.
:- dynamic spouse/2.
:- dynamic parent/2.

% Function to load relationships from a file
load_relationships(FileName) :-
    open(FileName, read, Stream),
    repeat,
    read_line_to_string(Stream, Line),
    (   Line == end_of_file
    ->  close(Stream), !
    ;   term_string(Term, Line),
        assertz(Term),
        fail
    ).

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

daughter_in_law(DaughterInLaw, Person) :-
    female(DaughterInLaw),
    parent(Person, Child),
    spouse(Child, DaughterInLaw),
    male(Child).

son_in_law(SonInLaw, Person) :-
    male(SonInLaw),
    parent(Person, Child),
    spouse(Child, SonInLaw),
    female(Child).




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
relationship(DIL, daughter_in_law, P) :- daughter_in_law(DIL, P).

relationship(SIL, son_in_law, P) :- son_in_law(SIL, P).



% Helper predicate to find the shortest path in terms of relationships
% Helper predicate to find the shortest path in terms of relationships
shortest_path(Start, Goal, Path) :-
    breadth_first([[Start]], Goal, [Start], RevPath),
    reverse(RevPath, Path).

% Breadth-first search helper predicate
breadth_first([[Goal | Path] | _], Goal, _, [Goal | Path]).
breadth_first([[Person | Path] | Paths], Goal, Visited, Solution) :-
    extend_path_with_visited(Person, Path, Visited, NewPaths, NewVisited),
    append(Paths, NewPaths, Paths1),
    breadth_first(Paths1, Goal, NewVisited, Solution).

% Extend a path to all directly related persons not yet in the path, tracking visited
extend_path_with_visited(Person, Path, Visited, NewPaths, NewVisited) :-
    findall([NewPerson, Person | Path],
            (relationship(Person, _, NewPerson), \+ member(NewPerson, Visited)),
            NewPaths),
    findall(NewPerson,
            (relationship(Person, _, NewPerson), \+ member(NewPerson, Visited)),
            NewNodes),
    append(Visited, NewNodes, NewVisited).

% Show the shortest relationship path between two persons
show_shortest_relationship(Person1, Person2) :-

    % Convert strings to atoms
    atom_string(Atom1, Person1),
    atom_string(Atom2, Person2),

    (   shortest_path(Atom1, Atom2, Path) ->
        write('The shortest relationship path from '), 
        write(Atom1), 
        write(' to '), 
        write(Atom2), 
        write(' is: '), 
        print_path(Path)
    ;   shortest_path(Atom2, Atom1, Path) ->
        write('The shortest relationship path from '), 
        write(Atom2), 
        write(' to '), 
        write(Atom1), 
        write(' is: '), 
        print_path(Path)
    ;   write('No relationship path found between '), 
        write(Atom1), 
        write(' and '), 
        write(Atom2), 
        write('.\n')
    ).

% Helper predicate to print the path of relationships in a sentence
print_path(Path) :-
    path_to_sentence(Path, Sentence), % Convert the path to a sentence
    writeln(Sentence). % Write the sentence to the output

% Convert the path of relationships into a sentence
path_to_sentence([], '').
path_to_sentence([Person], Person).
path_to_sentence([Person1, Person2], Sentence) :-
    relationship(Person1, Relation, Person2),
    format(atom(Sentence), '~w is ~w of ~w', [Person1, Relation, Person2]).
path_to_sentence([Person1, Person2, Person3 | Rest], Sentence) :-
    relationship(Person1, Relation1, Person2),
    relationship(Person2, Relation2, Person3),
    path_to_sentence([Person3 | Rest], RestSentence),
    format(atom(RelationPart), '~w is ~w of ~w who is ~w of ', [Person1, Relation1, Person2, Relation2]),
    string_concat(RelationPart, RestSentence, Sentence).



% Predicate to handle the natural language query
handle_query(Query) :-
    % Convert the query to lowercase for easier matching
    string_lower(Query, LowerQuery),
    % Use Prologs string matching to parse the query
    parse_query(LowerQuery, Person1, Person2),
    % Use the parsed names to show the relationship
    show_shortest_relationship(Person1, Person2).

% Parse the query to extract names based on expected sentence structure
parse_query(Query, Person1, Person2) :-
    % Split the query into a list of words
    split_string(Query, " ", "", List),
    
    
    % Find the position of 'is' and extract Person1
    nth0(Index1, List, "related"),
    nth1(Index1, List, Person1),

    % Find the position of 'to' and extract Person2
    nth0(Index2, List, "?"),
    nth1(Index2, List, Person2). % Person2 is right after 'to'


print_list([]). % Base case: empty list
print_list([Head|Tail]) :-
    writeln(Head),
    print_list(Tail).

% Start of the program
:- initialization(main).

main :-
    load_relationships('predicate_relations.txt'),
    writeln('Relationships loaded successfully. Ready for queries.'),
    writeln('Please enter your query:'),
    read_line_to_string(user_input, Query),
    handle_query(Query)
