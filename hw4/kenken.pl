use_module(library(clpfd)).

kenken_testcase(
  6,
  [
   +(11, [[1|1], [2|1]]),
   /(2, [1|2], [1|3]),
   *(20, [[1|4], [2|4]]),
   *(6, [[1|5], [1|6], [2|6], [3|6]]),
   -(3, [2|2], [2|3]),
   /(3, [2|5], [3|5]),
   *(240, [[3|1], [3|2], [4|1], [4|2]]),
   *(6, [[3|3], [3|4]]),
   *(6, [[4|3], [5|3]]),
   +(7, [[4|4], [5|4], [5|5]]),
   *(30, [[4|5], [4|6]]),
   *(6, [[5|1], [5|2]]),
   +(9, [[5|6], [6|6]]),
   +(8, [[6|1], [6|2], [6|3]]),
   /(2, [6|4], [6|5])
  ]
).

check(N, T) :-
  length(T, N),
  maplist(length_(N), T),
  flatten(T, V), fd_domain(V, 1, N),
  maplist(fd_all_different, T), checkColumns(N,T,0).

getColumn([], _, []).
getColumn([L1 | L2], C, [R | X]) :- 
  nth0(C, L1, R),
getColumn(L2, C, X).

checkColumns(N, _, N).
checkColumns(N, T, C) :-
  getColumn(T, C, R),
  fd_all_different(R),
  CN is C+1,
  checkColumns(N, T, CN).

label([]).
label([L1 | L2]) :-
  fd_labeling(L1), label(L2).

valid(N, T) :-
  check(N, T).

length_(L, Ls) :- length(Ls, L).

getAt(R, C, T, A) :-
  RP is R-1, CP is C-1,
  nth0(RP, T, Column), nth0(CP, Column, A).

testAdd(S, L, T) :-
  add(L, T, A), A #=# S.

add([], _, 0).
add([[R | C] | L2], T, S) :-
  getAt(R, C, T, N1), add(L2, T, N2),
  S #=# N2 + N1. 

testSubtract(D, [J|R], [V|P], T) :-
  getAt(J,R,T,N1), getAt(V,P,T,N2),
  (D #=# N1 - N2; D #=# N2 - N1).

testMultiply(P, L, T) :-
  mult(L, T, E), E #=# P.

mult([], _, 1).
mult([[R | C] | L2], T, P) :-
  getAt(R, C, T, N1), mult(L2, T, N2),
    P #=# N1 * N2. 

testDivision(Q, [R1|C1], [R2|C2], T) :-
  getAt(R1, C1, T, N1), getAt(R2,C2,T,N2),
  (Q #=# N1 / N2; Q #=# N2/N1).

checkKenken([], T).
checkKenken([ +(S, L) | L2], T) :-
  testAdd(S, L, T), checkKenken(L2, T).
checkKenken([ -(D, J, K) | L2], T) :-
  testSubtract(D, J, K, T), checkKenken(L2, T).
checkKenken([ *(P, L) | L2], T) :-
  testMultiply(P, L, T), checkKenken(L2, T).
checkKenken([ /(Q, J, K) | L2], T) :-
  testDivision(Q, J, K, T) , checkKenken(L2, T).

kenken(N, C, T) :-
  valid(N, T),
  checkKenken(C, T),
  label(T).






valid_plain(N, T) :-
  checkRows_plain(N, T), 
  checkColumns_plain(N, T).

checkRows_plain(N, [L1 | L2]) :-
  checkARow(N, L1, 0), checkRows_plain(N, L2).

checkARow(N, L, N).
checkARow(N, L, C) :-
  Np is C+1,
  member(Np, L), checkARow(N, L, Np).


checkColumns_plain(0, _).
checkColumns_plain(N, T) :-
  Np is N-1,
  getColumn(T, Np, C), checkARow(N, C, 0), checkColumns_plain(Np, T). 

plain_kenken(N, C, T) :-
  valid_plain(N, T),
  checkKenken_plain(C, T).
