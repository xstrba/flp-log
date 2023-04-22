graph(['A', 'B', 'C', 'D', 'E', 'F', 'G'], [
    ['A','B'],['C','A'],['B','E'],['C','E'],['D','E'],['D','F'],['F','G'],['G','B']
    ]).

reverse_all([], []).
reverse_all([[A,B]|T], [[B,A]|T2]) :- reverse_all(T,T2).

concat([], [], []).
concat([], [A|T2], [A|TR]) :- concat([], T2, TR).
concat([A|T],T2,[A|TR]) :- concat(T,T2,TR).

reverseAllEdges(RE) :- graph(_,GE), reverse_all(GE,RE).

allEdges(E) :- graph(_,GE) , reverse_all(GE, RE) , concat(GE,RE,E).

% path(B, A, [[A,B]]) :- graph(_,E) , reverse_all(E, R), concat(E, R, ER) , (member([A,B], ER)).
% path(A, B, [[A,B]]) :- graph(_,E) , reverse_all(E, R), ((member([A,B], E)) ; member([B,A], R)).

unfilteredPath(A, B, [[A,B]], V) :- allEdges(E)
                                        , (\+ member([A,B], V), \+ member([B,A], V))
                                        , member([A,B], E).
unfilteredPath(A, B, [[A,X]|LET], V) :- allEdges(E) , member([A,X], E)
                                        , (\+ member([A,X], V), \+ member([X,A], V))
                                        , X \= B
                                        , X \= A
                                        , unfilteredPath(X,B,LET, [[A,X]|V]).

filteredPath([], []).
filteredPath([A,B], [A,B]) :- graph(_,E) , member([A,B], E).
filteredPath([A,B], [B,A]) :- graph(_,E) , member([B,A], E).
filteredPath([[A,B]|XU], [[A,B]|XF]) :- graph(_,E) , member([A,B], E) , filteredPath(XU, XF).
filteredPath([[A,B]|XU], [[B,A]|XF]) :- graph(_,E) , member([B,A], E) , filteredPath(XU, XF).

path(A,B,XF) :- unfilteredPath(A,B,XU,[]) , filteredPath(XU, XF).