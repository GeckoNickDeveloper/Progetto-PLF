dft([], []).
dft([X | Y], C) :-  size([X | Y], S),
                    generate_dft([X | Y], S, 0, C).
                    

generate_dft([], _, _, []).
generate_dft(L, S, K, C) :- ((K == S) -> C = [];
                                  K1 is K + 1,
                                  generate_dft(L, S, K1, C1),
                                  sum_terms_dft(L, S, K, 0, E),
                                  append([E], C1, C)).

sum_terms_dft([], _, _, _, (0,0)).
sum_terms_dft([H | T], S, K, N, C) :- 
      N1 is N + 1,
      O is (-((2 * pi) / S) * N * K),
      sum_terms_dft(T, S, K, N1, C1),
      complex_prod(H, (cos(O),sin(O)), P),
      complex_sum(P, C1, C).



size([], 0).
size([_ | Y], S) :- size(Y, S1),
                    S is S1 + 1.


complex_prod((ZR, ZI), (WR, WI), (R,I)) :-  R is (ZR * WR - ZI * WI),
                                            I is (ZR * WI + ZI * WR).

complex_sum((ZR, ZI), (WR, WI), (R,I)) :- R is ZR + WR, I is ZI + WI.