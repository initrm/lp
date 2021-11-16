-module(ex1).
-export([is_palindrome/1, is_an_anagram/2, factors/1, is_proper/1]).

% remove_special : char → boolean
% returns true if the given char is a letter and false otherwise
is_alpha(C) when (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z) -> true;
is_alpha(_) -> false.

% remove_special : string → string
% removes special chars from a string
remove_specials(S) -> [L || L <- S, is_alpha(L) ].

% is_palindrome: string → bool 
% that checks if the string given as input is palindrome, 
% a string is palindrome when the represented sentence can be read the same way 
% in either directions in spite of spaces, punctual and letter cases;
pvt_palindrome([]) -> true;
pvt_palindrome([_|[]]) -> true;
pvt_palindrome([H|TL]) ->
    case H == lists:nth(1, string:reverse(TL)) of
        true -> pvt_palindrome(string:substr(TL, 1, string:len(TL) - 1));
        false -> false
    end.

is_palindrome(S) -> pvt_palindrome(string:to_lower(remove_specials(S))).

% is_an_anagram : string → string list → boolean 
% given a dictionary of strings, checks if the input string is an anagram of one 
% or more of the strings in the dictionary;
is_an_anagram(_, []) -> false;
is_an_anagram(S, [H|TL]) -> 
    case string:len(S--H) of
        0 -> true;
        _ -> is_an_anagram(S, TL)
    end.

% factors: int → int list 
% given a number calculates all its prime factors;
factors(N) -> factors(N, 2).

factors(N, CP) when N == 0 orelse CP > N -> [];
factors(N, CP) when (N rem CP) == 0 -> [CP|factors(N div CP, CP)];
factors(N, CP) -> factors(N, next_prime(CP)).

% is_prime: int → boolean
% returns true if the given number is prime
is_prime(N) -> 
    case N of
        0 -> false;
        1 -> true;
        _ -> is_prime(N, 2)
    end.

is_prime(N, D) when D == N -> true;
is_prime(N, D) when (N rem D) == 0 -> false;
is_prime(N, D) -> is_prime(N, D+1).

% next_prime_number: int → int
% calculates the nearest greater prime number from N;
next_prime(N) ->
    case is_prime(N+1) of
        true -> N+1;
        false -> next_prime(N+1)
    end.

% is_proper: int → boolean 
% given a number calculates if it is a perfect number or not, where a perfect number 
% is a positive integer equal to the sum of its proper positive divisors (excluding itself), 
%   e.g., 6 is a perfect number since 1, 2 and 3 are the proper divisors of 6 and 6 is equal to 1+2+3;
is_proper(N) -> reduce(divisors(N)) == N.

% reduce: int list → int
% calculates the sum of every element in the list;
reduce([]) -> 0;
reduce([HD|TL]) -> HD + reduce(TL).

% divisors: int → int list
% returns a list containing all the divisors of N except and itself;
divisors(N) -> divisors(N, 1).

divisors(N, CD) when CD > (N div 2) -> [];
divisors(N, CD) when N rem CD == 0 -> [CD|divisors(N, CD+1)];
divisors(N, CD) -> divisors(N, CD+1).