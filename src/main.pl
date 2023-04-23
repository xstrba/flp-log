%#########################################
% @file main.pl                          #
% @author Boris Štrbák (xstrba05)        #
%                                        #
% Parse graph data from input file       #
% Store nodes and edges in graph         #
% predicate                              #
% Find all hamiltonian paths in graph    #
% starting from first node in nodes list #
%#########################################

:- dynamic nodes/1 , edge/2.

unfilteredPath(A, B, [[A,B]], V) :- (\+ member([A,B], V), \+ member([B,A], V))
                                , edge(A,B).

unfilteredPath(A, B, [[A,X]|LET], V) :- edge(A,X)
                                    , (\+ member([A,X], V), \+ member([X,A], V))
                                    , X \= B
                                    , X \= A
                                    , unfilteredPath(X,B,LET, [[A,X]|V]).

filteredPath([], []) :- !.
filteredPath([A,B], [A,B]) :- sort([A,B], [A,B]) , edge(A,B).
filteredPath([A,B], [B,A]) :- sort([B,A], [B,A]) , edge(B,A).
filteredPath([[A,B]|XU], [[A,B]|XF]) :- edge(A,B), sort([A,B], [A,B]) , filteredPath(XU, XF).
filteredPath([[A,B]|XU], [[B,A]|XF]) :- edge(B,A), sort([B,A], [B,A]) , filteredPath(XU, XF).

path(A,B,XF) :- unfilteredPath(A,B,XU,[]) , filteredPath(XU, XFUNS) , sort(XFUNS, XF).

paths(A,B,X) :- bagof(Path, path(A,B,Path), Paths) , sort(Paths, X).


filterPaths(All, N, XF) :-
    include(hasAllNodes(N), All, XF).

hasAllNodes(N, X) :-
    flatten(X, XF),
    sort(XF, UN),
    subset(N, UN).

uniquePaths(List, Result) :-
    uniquePaths(List, [], Result).

uniquePaths([], Acc, Acc).
uniquePaths([H|T], Acc, Result) :-
    (member(H, Acc) ; member(RevH, Acc), reverse(H, RevH)),
    !, uniquePaths(T, Acc, Result).
uniquePaths([H|T], Acc, Result) :-
    \+ (member(H, Acc) ; member(RevH, Acc), reverse(H, RevH)),
    uniquePaths(T, [H|Acc], Result).

path2(A,B,X) :- nodes(Nodes)
                , paths(A,B,All)
                , filterPaths(All, Nodes, Filtered)
                , uniquePaths(Filtered, UPaths)
                , member(X, UPaths).

readLine(L,C) :-
    get_char(C),
    (isEOFEOL(C), L = [], !;
    readLine(LL, _),
    [C|LL] = L ).
    isEOFEOL(C) :-
    C == end_of_file;
    (char_code(C,Code), Code==10).

readLines(Ls) :-
    readLine(L, C),
    (C == end_of_file, Ls=[] ;
    (readLines(LLs), [L|LLs] = Ls)).

writeLines(_, []) :- !.
writeLines(OUT, [[A,' ',B]|LLs]) :- char_type(A, upper)
    , char_type(B, upper)
    , string_chars(STR, [A, '-', B])
    , write(OUT, STR)
    , write(OUT, "\n")
    , writeLines(OUT, LLs) , !.
writeLines(OUT, [_|LLs]) :- writeLines(OUT, LLs) , !.

readData(_, [], []) :- !.
readData(OUT, [[A,' ',B]|LLs], [A,B|V]) :-
    char_type(A, upper)
    , char_type(B, upper)
    , assertz(edge(A,B))
    , assertz(edge(B,A))
    , readData(OUT, LLs, V), !.
readData(OUT, [_|LLs], V) :- readData(OUT, LLs, V) , !.

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

main :- readLines(Ls) , readData(current_output, Ls, V) , sort(V, [A|SV])
    , assertz(nodes([A|SV]))
    , bagof(Path, path2(A, A, Path), Paths)
    , writePaths(current_output, Paths) , !. 