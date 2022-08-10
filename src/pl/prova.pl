dct([], []).
dct([X | Y], C) :-  size([X | Y], S),
                    generate_dct([X | Y], S, 0, C).
                    

generate_dct([], _, _, []).
generate_dct(L, S, K, C) :- ((K == S) -> C = [];
                                  K1 is K + 1,
                                  /*write(K), nl,*/
                                  generate_dct(L, S, K1, C1),
                                  sum_terms_dct(L, S, K, 0, E),
                                  write(E), nl,
                                  append([E], C1, C)).

sum_terms_dct([], _, _, _, 0.0).
sum_terms_dct([H | T], S, K, N, C) :- 
      N1 is N + 1,
      sum_terms_dct(T, S, K, N1, C1),
      C is C1 + (H * 2 * cos(pi * (2 * N + 1) * K / (2 * S))).



size([], 0).
size([_ | Y], S) :- size(Y, S1),
                    S is S1 + 1.