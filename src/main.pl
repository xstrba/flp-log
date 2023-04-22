% graph(['A', 'B', 'C', 'D', 'E', 'F', 'G'], [
%     ['A','B'],['C','A'],['B','E'],['C','E'],['D','E'],['D','F'],['F','G'],['G','B']
%     ]).

% graph(['A', 'B', 'C', 'D'], [
%     ['A','B'],['A','C'], ['A','D'], ['A','B'], ['B','C'], ['B','D'],['C','D']
%     ]).

:- dynamic graph/2.


reverseAll([], []).
reverseAll([[A,B]|T], [[B,A]|T2]) :- reverseAll(T,T2).

concat([], [], []).
concat([], [A|T2], [A|TR]) :- concat([], T2, TR).
concat([A|T],T2,[A|TR]) :- concat(T,T2,TR).

reverseAllEdges(RE) :- graph(_,GE), reverseAll(GE,RE).

allEdges(E) :- graph(_,GE) , reverseAll(GE, RE) , concat(GE,RE,EUNS) , sort(EUNS, E).

unfilteredPath(A, B, [[A,B]], V) :- allEdges(E)
                                , (\+ member([A,B], V), \+ member([B,A], V))
                                , member([A,B], E).
unfilteredPath(A, B, [[A,X]|LET], V) :- allEdges(E) , member([A,X], E)
                                    , (\+ member([A,X], V), \+ member([X,A], V))
                                    , X \= B
                                    , X \= A
                                    , unfilteredPath(X,B,LET, [[A,X]|V]).

filteredPath([], []) :- !.
filteredPath([A,B], [A,B]) :- graph(_,E) , member([A,B], E).
filteredPath([A,B], [B,A]) :- graph(_,E) , member([B,A], E).
filteredPath([[A,B]|XU], [[A,B]|XF]) :- graph(_,E) , member([A,B], E) , filteredPath(XU, XF).
filteredPath([[A,B]|XU], [[B,A]|XF]) :- graph(_,E) , member([B,A], E) , filteredPath(XU, XF).

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

path2(A,B,X) :- graph(Nodes, _)
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

readData(_, [], [], []) :- !.
readData(OUT, [[A,' ',B]|LLs], [A,B|V], [[A,B]|E]) :-
    char_type(A, upper)
    , char_type(B, upper)
    , readData(OUT, LLs, V, E), !.
readData(OUT, [_|LLs], V, E) :- readData(OUT, LLs, V, E) , !.

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

main :- readLines(Ls) , readData(current_output, Ls, V, E) , sort(V, [A|SV]) , sort(E, SE)
    , assertz(graph([A|SV], SE))
    , bagof(Path, path2(A, A, Path), Paths)
    , writePaths(current_output, Paths).