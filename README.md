# Shortest Family Chain of Relationship in Prolog
**By : Anshul Rana and Priyansh Desai**

The Prolog program is designed to answer queries about relationships between individuals. This report provides an overview of the program's functionality, implementation details, and explanation of how it generates the shortest possible chain of relations.

### How to run the code :

1. clone the git repository
    
    ```bash
    git clone https://github.com/anshulranaa/Shortest_Family_Relationship/
    ```
    

1. cd into the cloned repo folder
    
    ```bash
    	cd Shortest_Family_Relationship
    ```
    

1. Open the terminal and type
    
    ```bash
    swipl family.pl
    ```
    
2. Then type your query in this format :
    
    ```bash
    How is <Person1> related to <Person2><space>?
    ```
    
    For example : 
    
    ```bash
    How is katie related to sushi ?
    ```
    

**Program Overview**

The program is structured into several sections:

- **Relationship Definitions**: Defines various predicates for relationships such as male,female,spouse and parent.
- **Shortest Path Generation**: Implements a breadth-first search algorithm to find the shortest path between two individuals in terms of their relationships.
    
    ```prolog
    % Breadth-first search helper predicate
    breadth_first([[Goal | Path] | _], Goal, _, [Goal | Path]).
    breadth_first([[Person | Path] | Paths], Goal, Visited, Solution) :-
        extend_path_with_visited(Person, Path, Visited, NewPaths, NewVisited),
        append(Paths, NewPaths, Paths1),
        breadth_first(Paths1, Goal, NewVisited, Solution).
    ```
    
- **Query Handling**: Parses user queries, extracts individuals involved, and invokes the shortest path generation to answer the query.
    
    ```prolog
    % Predicate to handle the natural language query
    handle_query(Query) :-
        string_lower(Query, LowerQuery),
        parse_query(LowerQuery, Person1, Person2),
        show_shortest_relationship(Person1, Person2).
    
    % Parse the query to extract names based on expected sentence structure
    parse_query(Query, Person1, Person2) :-
        split_string(Query, " ", "", List),
        
        
        % Find the position of 'is' and extract Person1
        nth0(Index1, List, "related"),
        nth1(Index1, List, Person1),
    
        % Find the position of 'to' and extract Person2
        nth0(Index2, List, "?"),
        nth1(Index2, List, Person2). 

    ```
    
- **Main Loop**: Facilitates interaction with the user by continuously prompting for queries until the user chooses to exit.
    
    ```prolog
    % Start of the program
    :- initialization(main).
    
    main :-
        load_relationships('predicate_relations.txt'),
        writeln('Relationships loaded successfully. Ready for queries.'),
        writeln('Please enter your query:'),
        read_line_to_string(user_input, Query),
        handle_query(Query)
    
    ```
    

**3. Implementation Details**

- **Loading Relationships**: The program loads relationship data from a file (**`predicate_relations.txt`**) which contains facts about individuals, their genders, spouses, and parent-child relationships.
    
    ```prolog
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
    
    ```
    
- **Relationship Definitions**: Predicates are defined for various relationships such as parent, sibling, uncle, aunt, etc. These predicates utilize existing relationships and gender information to infer additional relationships.
    
    ```prolog
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
    
    ```
    
- **Shortest Path Generation**: The program implements a breadth-first search algorithm to find the shortest path between two individuals in terms of relationships. It extends paths by considering all directly related individuals not yet in the path until the target individual is reached.
- **Query Handling**: User queries are parsed to extract individuals involved and then the shortest path generation algorithm is invoked to find the shortest chain of relations between them.
- **Main Loop**: The main loop continuously prompts the user for queries, handles each query, and repeats until the user chooses to exit.

**4. Generating the Shortest Chain of Relations**

To generate the shortest possible chain of relations between two individuals:

- **Breadth-First Search**: The program uses a breadth-first search algorithm to explore the relationship graph.
- **Path Extension**: Starting with the initial individual, the algorithm extends paths by considering all directly related individuals not yet visited.
- **Goal State**: The algorithm terminates when the target individual is reached.
- **Optimality**: Breadth-first search ensures optimality in finding the shortest path as it explores all possible paths level by level.
