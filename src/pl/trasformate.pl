/*main :- 
	nl,
	write('...'), nl,
	write('...'), nl,
	write('...'), nl,
	write('...'), nl,
  dct([1.0, 3.7, 10.9, -2.6, 0, 0, 0.5, 14, 13.9], R),
  write(R).



*/

dct([], []).
dct([X | Y], C) :-  generate_dct([X | Y], S, 0, C),
                    size([X | Y], S).

generate_dct([], _, _, []).
generate_dct(L, S, K, C) :- ((K == S) -> C1 = [];
                                  generate_dct(L, S, K1, C1),
                                  K1 is K + 1,
                                  append(C1, E, C),
                                  sum_terms_dct(L, S, K, 0, E)).


sum_terms_dct([], _, _, _, 0.0).
sum_terms_dct([X | Y], S, K, N, E) :- sum_terms_dct(Y, S, K, N1, E1),
                                      N1 is N + 1,
                                      write(S),
                                      write(' '),
                                      write(N),
                                      write(' '),
                                      write(K), nl,
                                      E is E1 + (2 * X * cos(pi * (2 * N + 1) * K) / (2 * S)).

size([], 0).
size([_ | Y], S) :- size(Y, S1),
                    S is S1 + 1.