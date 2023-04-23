%#########################################
% Hamiltonovská kružnice                 #
%                                        #
% @file main.pl                          #
% @author Boris Štrbák (xstrba05)        #
%                                        #
% Parse graph data from input file       #
% Store nodes and edges in graph         #
% predicate                              #
% Find all hamiltonian paths in graph    #
% starting from first node in nodes list #
%#########################################

:- dynamic nodes/1 , edge/2, node/1.

% third argument contains all the paths from A to B
% fourth argument is for collecting visited edges.
% every edge will be in path only once.
unfilteredPath(A, B, [[A,B]], V) :- (\+ member([A,B], V), \+ member([B,A], V))
    , edge(A,B).
unfilteredPath(A, B, [[A,X]|LLs], V) :- edge(A,X)
    , (\+ member([A,X], V), \+ member([X,A], V))
    , X \= B
    , X \= A
    , unfilteredPath(X, B, LLs, [[A,X]|V]).

% second argument contains paths from first argument that have all
% edges (items) with nodes in sorted order
filteredPath([], []) :- !.
filteredPath([A,B], [A,B]) :- sort([A,B], [A,B]) , edge(A, B).
filteredPath([A,B], [B,A]) :- sort([B,A], [B,A]) , edge(B, A).
filteredPath([[A,B]|LPu], [[A,B]|LPf]) :- edge(A, B), sort([A,B], [A,B]) , filteredPath(LPu, LPf).
filteredPath([[A,B]|LPu], [[B,A]|LPf]) :- edge(B, A), sort([B,A], [B,A]) , filteredPath(LPu, LPf).

% Pf contains list of edges from A to B, where items in edge are sorted (e.g ['A', 'B'] and not ['B', 'A'])
% edges are also sorted and unique
path(A, B, Pf) :- unfilteredPath(A, B, Pu, []) , filteredPath(Pu, Pfu) , sort(Pfu, Pf).

% XPs contains all paths from A to B sorted and unique
paths(A, B, XPs) :- bagof(P, path(A, B, P), Ps) , sort(Ps, XPs).

% filter only paths that have all nodes
filterPaths(All, N, Pf) :- include(hasAllNodes(N), All, Pf). 

% function that checks if given path P
% has all nodes N
hasAllNodes(N, P) :- flatten(P, NP)
    , sort(NP, NPs)
    , subset(N, NPs).

% helper for reversing order of items in list
reverse([], Lr, Lr).
reverse([H|T], Acc, Lr) :- reverse(T, [H|Acc], Lr).

% reverse list function with only two params
reverseL2(L, Lr) :- reverse(L, [], Lr).

% function for removing similiar paths from list given by first argument
% second argument is accumulator and third argument contains
% only unique paths (list of edges). Paths in reverse order are considered same.
uniquePaths([], Lu, Lu).
uniquePaths([H|T], Acc, Lu) :- (member(H, Acc) ; member(RevH, Acc), reverse(H, RevH))
    , uniquePaths(T, Acc, Lu) , !.
uniquePaths([H|T], Acc, Lu) :- uniquePaths(T, [H|Acc], Lu) , !.

% same as uniquePaths but called only with two terms
uniquePaths2(L, Lu) :- uniquePaths(L, [], Lu).

% better function that finds all unique paths from A to B
path2(A, B, XP) :- nodes(N)
    , paths(A, B, All)
    , filterPaths(All, N, Pfs)
    , uniquePaths2(Pfs, Pfsur)
    , reverseL2(Pfsur, Pfsu)
    , member(XP, Pfsu).

% from 4th lecture
readLine(L, C) :-
    get_char(C),
    (
        isEOFEOL(C), L = [], !;
        readLine(LL, _),
        [C|LL] = L
    ) . isEOFEOL(C)
    :- C == end_of_file
    ; (char_code(C,Code), Code==10).

% from 4th lecture
readLines(Ls) :- readLine(L, C)
    , (C == end_of_file, Ls=[]
    ; (readLines(LLs), [L|LLs] = Ls)).

% read data from list of lines
% create dynamic predicates for nodes and edges
readData([]) :- !.
readData([[A,' ',B]|LLs]) :-
    char_type(A, upper)
    , char_type(B, upper)
    , (\+ node(A) -> assertz(node(A)) ; true)
    , (\+ node(B) -> assertz(node(B)) ; true)
    , (\+ edge(A,B) -> assertz(edge(A,B)) ; true)
    , (\+ edge(B,A) -> assertz(edge(B,A)) ; true)
    , readData(LLs), !.
readData([_|LLs]) :- readData(LLs) , !.

% functions for printing path to output
writeEdge(OUT, A, B) :-
    string_chars(STR, [A, '-', B])
    , write(OUT, STR) , !.

writePath(_, []) :- !.
writePath(OUT, [[A,B]]) :-
    writeEdge(OUT, A, B)
    , write(OUT, "\n") , !.

writePath(OUT, [[A,B]|LLs]) :-
    writeEdge(OUT, A, B)
    , write(OUT, ' ')
    , writePath(OUT, LLs) , !.

writePaths(_, []) :- !.
writePaths(OUT, [L|LLs]) :-
    writePath(OUT, L)
    , writePaths(OUT, LLs) , !.

% end of functions for printing path to output

% main function that will read input, process lines of input,
% add dynamic predicate returning true for all the nodes,
% and find all paths from first node back to itself using path2
% these paths should contain only unique edges.
% Then it will print out paths.
main :- readLines(Ls)
    , readData(Ls)
    , bagof(N, node(N), Ns)
    , sort(Ns, [A|SV])
    , assertz(nodes([A|SV]))
    , (bagof(P, path2(A, A, P), Ps) -> (
        writePaths(current_output, Ps) , !
    ) ; true) , !.