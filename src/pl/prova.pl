sum_terms_dct([], _, _, _, 0.0).
sum_terms_dct([H | T], S, K, N, C) :- 
      N1 is N + 1,
      sum_terms_dct(T, S, K, N1, C1),
      C is C1 + (H * 2 * cos(pi * (2 * N + 1) * K / (2 ))),
      write(S),
      write(' '),
      write(K),
      write(' '),
      write(N), nl.



size([], 0).
size([_ | Y], S) :- size(Y, S1),
                    S is S1 + 1.