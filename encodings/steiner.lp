edge(X,Y) :- edge(Y,X).
vertex(X) :- oedge(X,_).
vertex(Y) :- oedge(_,Y).

oedge(X,Y) :- edge(X,Y), X < Y.

%oweight(X,Y,W) :- oedge(X,Y), eweight(X,Y,W).
%oweight(X,Y,W) :- oedge(X,Y), eweight(Y,X,W).

0 { p(X,Y) } 1 :- oedge(X,Y).

% For all partitions into two sets there must be a crossing edge.
s1(X) | s2(X) :- vertex(X).

s1(X) :- saturate, vertex(X).
s2(X) :- saturate, vertex(X).
:- not saturate.

% Found crossing edge?
saturate :- p(X,Y), s1(X), s2(Y), X < Y.
saturate :- p(X,Y), s2(X), s1(Y), X < Y.

% Not a partition due to a set being terminal empty?
% This is the case if the other set contains all terminal elements.
%numVertices(N) :- N = #count{ X : terminalVertex(X) }.
saturate :- N #count{ X : s1(X), terminalVertex(X) }, numVertices(N).
saturate :- N #count{ X : s2(X), terminalVertex(X) }, numVertices(N).

%chosen :- s2(X), terminalVertex(X).
%chosen :- s1(X), terminalVertex(X).
%saturate :- not chosen.

#show p/2.

#minimize { 1,V1,V2 : p(V1,V2) }.
%#minimize { W,V1,V2 : p(V1,V2), oweight(V1,V2,W) }.
