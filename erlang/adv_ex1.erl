-module(adv_ex1).
-export([squared_int/1, intersect/2, symmetric_difference/2]).

% removes all non-integers from a polymorphic list and returns the resulting list of integers squared
squared_int(L) -> [X*X || X <- L, is_integer(X)].

% verifies if the list contains the provided value
contains([HD|_], N) when HD == N -> true;
contains([_|TL], N) -> contains(TL, N);
contains([], _) -> false.

% given two lists, returns a new list that is the intersection of the two lists
intersect(A, B) -> [X || X <- A, contains(A, X) andalso contains(B, X)].

% given two lists, returns a new list that is the symmetric difference of the two lists
symmetric_difference(A, B) -> [X || X <- (A--B)++(B--A)].