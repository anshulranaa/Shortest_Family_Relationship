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

% Processing the user query
process_query :-
    write('Please enter your query (e.g., "How is X related to Y"): '),
    read_line_to_string(user_input, Query),
    % Parsing the query
    split_string(Query, " ", "", Words),
    nth0(2, Words, Person1), % Extracts the third word (assuming "How is X related to Y")
    nth0(5, Words, Person2), % Extracts the sixth word
    % Trim any potential whitespace
    string_lower(Person1, Lower1),
    string_lower(Person2, Lower2),
    % Displaying the relationship path
    show_shortest_relationship(Lower1, Lower2).

% Convert the person names to lowercase before comparing, assuming all stored names are in lowercase
string_lower(Str, Lower) :-
    string_codes(Str, Codes),
    maplist(lower_code, Codes, LowerCodes),
    string_codes(Lower, LowerCodes).

lower_code(Code, LowerCode) :-
    (   Code >= 65, Code =< 90
    ->  LowerCode is Code + 32
    ;   LowerCode = Code
    ).


% Start of the program
:- initialization(main).

main :-
    load_relationships('predicate_relations.txt'),
    writeln('Relationships loaded successfully. Ready for queries.'),
    repeat,
    process_query,
    writeln('Enter "exit" to quit or "continue" to make another query.'),
    read_line_to_string(user_input, Response),
    Response == "exit", !.