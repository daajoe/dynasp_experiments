edge(X,Y) :- edge(Y,X).

{ selectedEdge(X,Y) : edge(X,Y), X < Y }.

connected(X,Y) :- selectedEdge(X,Y), X < Y.

connected(X,Z) :- connected(X,Y), selectedEdge(Y,Z), X != Z, Y != Z, Y < Z.
connected(X,Z) :- connected(X,Y), selectedEdge(Z,Y), X != Z, Y != Z, Z < Y.
connected(X,Z) :- connected(Y,Z), selectedEdge(X,Y), X != Z, Y != Z, X < Y.
connected(X,Z) :- connected(Y,Z), selectedEdge(Y,X), X != Z, Y != Z, Y < X.

:- terminalVertex(X), terminalVertex(Y), X < Y, not connected(X,Y), not connected(Y,X).

relevantWeight(V1,V2,1) :- selectedEdge(V1,V2), V1 < V2.
#minimize { 1,V1,V2 : relevantWeight(V1,V2,W) }.

#show selectedEdge/2.


%eweight(X,Y,W) :- eweight(Y,X,W).
%relevantWeight(V1,V2,W) :- selectedEdge(V1,V2), eweight(V1,V2,W), V1 < V2.
%#minimize { W,V1,V2 : relevantWeight(V1,V2,W) }.


