color(red;green).

1 { map(X,C) : color(C) } 1 :- vertex(X).
:- edge(X,Y), map(X,green), map(Y,green).
#minimize { 1,X : map(X,red) }.
#show map/2.
