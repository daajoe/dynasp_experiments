0 { p(X,Y) } 1 :- edge(X,Y).

% For all partitions into two sets there must be a crossing edge.
s1(X) | s2(X) :- vertex(X).
s1(X) :- saturate, vertex(X).
s2(X) :- saturate, vertex(X).
:- not saturate.

% Found crossing edge?
saturate :- p(X,Y), s1(X), s2(Y).
saturate :- p(Y,X), s1(X), s2(Y).

% Not a partition due to a set being terminal empty?
% This is the case if the other set contains all terminal elements.
numVertices(N) :- N = #count{ X : terminalVertex(X) }.
saturate :- N #count{ X : s1(X), terminalVertex(X) }, numVertices(N).
saturate :- N #count{ X : s2(X), terminalVertex(X) }, numVertices(N).

#minimize { 1,X,Y : p(X,Y) }.

#show p/2.
