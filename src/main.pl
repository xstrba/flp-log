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

unfilteredPath(A, B, [[A,B]], V) :- (\+ member([A,B], V), \+ member([B,A], V))
    , edge(A,B).

unfilteredPath(A, B, [[A,X]|LLs], V) :- edge(A,X)
    , (\+ member([A,X], V), \+ member([X,A], V))
    , X \= B
    , X \= A
    , unfilteredPath(X,B,LLs, [[A,X]|V]).

filteredPath([], []) :- !.
filteredPath([A,B], [A,B]) :- sort([A,B], [A,B]) , edge(A, B).
filteredPath([A,B], [B,A]) :- sort([B,A], [B,A]) , edge(B, A).
filteredPath([[A,B]|LPu], [[A,B]|LPf]) :- edge(A, B), sort([A,B], [A,B]) , filteredPath(LPu, LPf).
filteredPath([[A,B]|LPu], [[B,A]|LPf]) :- edge(B, A), sort([B,A], [B,A]) , filteredPath(LPu, LPf).

path(A, B, Pf) :- unfilteredPath(A, B, Pu, []) , filteredPath(Pu, Pfu) , sort(Pfu,Pf).

paths(A, B, XPs) :- bagof(P, path(A, B, P), Ps) , sort(Ps, XPs).

filterPaths(All, N, Pf) :- include(hasAllNodes(N), All, Pf). 

hasAllNodes(N, P) :- flatten(P, NP)
    , sort(NP, NPs)
    , subset(N, NPs).

reverse([], Lr, Lr).
reverse([H|T], Acc, Lr) :- reverse(T, [H|Acc], Lr).

reverseL2(L, Lr) :- reverse(L, [], Lr).

uniquePaths([], Lu, Lu).
uniquePaths([H|T], Acc, Lu) :- (member(H, Acc) ; member(RevH, Acc), reverse(H, RevH))
    , uniquePaths(T, Acc, Lu) , !.
uniquePaths([H|T], Acc, Lu) :- uniquePaths(T, [H|Acc], Lu) , !.

uniquePaths2(L, Lu) :- uniquePaths(L, [], Lu).

path2(A, B, XP) :- nodes(N)
    , paths(A, B, All)
    , filterPaths(All, N, Pfs)
    , uniquePaths2(Pfs, Pfsur)
    , reverseL2(Pfsur, Pfsu)
    , member(XP, Pfsu).

readLine(L, C) :-
    get_char(C),
    (
        isEOFEOL(C), L = [], !;
        readLine(LL, _),
        [C|LL] = L
    ) . isEOFEOL(C)
    :- C == end_of_file
    ; (char_code(C,Code), Code==10).

readLines(Ls) :- readLine(L, C)
    , (C == end_of_file, Ls=[]
    ; (readLines(LLs), [L|LLs] = Ls)).

readData(_, []) :- !.
readData(OUT, [[A,' ',B]|LLs]) :-
    char_type(A, upper)
    , char_type(B, upper)
    , (\+ node(A) -> assertz(node(A)) ; true)
    , (\+ node(B) -> assertz(node(B)) ; true)
    , (\+ edge(A,B) -> assertz(edge(A,B)) ; true)
    , (\+ edge(B,A) -> assertz(edge(B,A)) ; true)
    , readData(OUT, LLs), !.
readData(OUT, [_|LLs]) :- readData(OUT, LLs) , !.

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

main :- readLines(Ls) , readData(current_output, Ls)
    , bagof(N, node(N), Ns)
    , sort(Ns, [A|SV])
    , assertz(nodes([A|SV]))
    , (bagof(P, path2(A, A, P), Ps) -> (
        writePaths(current_output, Ps) , !
    ) ; true) , !.