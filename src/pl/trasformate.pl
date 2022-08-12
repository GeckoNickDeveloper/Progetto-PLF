/* Main */
main :-
  nl,
  write('Progetto PLF'), nl, nl, nl,
  acquire_real_list(RealList), nl,
  dct(RealList, C),
  write('Risultato DCT:'), nl,
  write(C), nl, nl,
  idct(C, DctOriginal),
  write('Risultato IDCT:'), nl,
  write(DctOriginal), nl, nl,nl,
  
  acquire_complex_list(ComplexList), nl,
  dft(ComplexList, X),
  write('Risultato DFT:'), nl,
  write(X), nl, nl,
  idft(X, DftOriginal),
  write('Risultato IDFT:'), nl,
  write(DftOriginal), nl.


/* DCT */
dct([], []).
dct([X | Y], C) :-  size([X | Y], S),
                    generate_dct([X | Y], S, 0, C).
                    

generate_dct([], _, _, []).
generate_dct(L, S, K, C) :- ((K == S) -> C = [];
                                  K1 is K + 1,
                                  generate_dct(L, S, K1, C1),
                                  sum_terms_dct(L, S, K, 0, E),
                                  append([E], C1, C)).

sum_terms_dct([], _, _, _, 0.0).
sum_terms_dct([H | T], S, K, N, C) :- 
      N1 is N + 1,
      sum_terms_dct(T, S, K, N1, C1),
      C is C1 + (H * 2 * cos(pi * (2 * N + 1) * K / (2 * S))).




/* IDCT */
idct([], []).
idct([X | Y], C) :- size([X | Y], S),
                    generate_idct([X | Y], S, 0, C).
                    

generate_idct([], _, _, []).
generate_idct([H | T], S, N, C) :- ((N == S) -> C = [];
                                  N1 is N + 1,
                                  generate_idct([H | T], S, N1, C1),
                                  sum_terms_idct(T, S, N, 1, E),
                                  X is (H + E) / (2 * S),
                                  append([X], C1, C)).

sum_terms_idct([], _, _, _, 0.0).
sum_terms_idct([H | T], S, N, K, C) :- 
      K1 is K + 1,
      sum_terms_idct(T, S, N, K1, C1),
      C is C1 + (H * 2 * cos(pi * (2 * N + 1) * K / (2 * S))).






/* DFT */
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
      THETA is (-((2 * pi) / S) * N * K),
      sum_terms_dft(T, S, K, N1, C1),
      complex_prod(H, (cos(THETA),sin(THETA)), P),
      complex_sum(P, C1, C).

/* IDFT */
idft([], []).
idft([X | Y], C) :-  size([X | Y], S),
                    generate_idft([X | Y], S, 0, C).
                    

generate_idft([], _, _, []).
generate_idft(L, S, N, C) :- ((N == S) -> C = [];
                                  N1 is N + 1,
                                  generate_idft(L, S, N1, C1),
                                  sum_terms_idft(L, S, N, 0, E),
                                  complex_div_real(E, S, P),
                                  append([P], C1, C)).

sum_terms_idft([], _, _, _, (0,0)).
sum_terms_idft([H | T], S, N, K, C) :- 
      K1 is K + 1,
      THETA is (((2 * pi) / S) * N * K),
      sum_terms_idft(T, S, N, K1, C1),
      complex_prod(H, (cos(THETA),sin(THETA)), P),
      complex_sum(P, C1, C).

/* Auxilary */
size([], 0).
size([_ | Y], S) :- size(Y, S1),
                    S is S1 + 1.

complex_prod((ZR, ZI), (WR, WI), (R,I)) :-  R is (ZR * WR - ZI * WI),
                                            I is (ZR * WI + ZI * WR).

complex_sum((ZR, ZI), (WR, WI), (R,I)) :- R is ZR + WR, I is ZI + WI.

complex_div_real((ZR, ZI), N, (R, I)) :- R is ZR / N, I is ZI / N.


acquire_real_list(List) :-
  write('Inserisci una lista di numeri reali:'), nl,
  read(List),
  ((check_real_list(List)) -> !; write('Errore.'), nl, acquire_real_list(List)).

acquire_complex_list(List) :-
  write('Inserisci una lista di numeri complessi:'), nl,
  read(List),
  ((check_complex_list(List)) -> !; write('Errore.'), nl, acquire_complex_list(List)).

check_real_list([]).
check_real_list([A | L]) :- ((number(A)) -> check_real_list(L); fail).

check_complex_list([]).
check_complex_list([(A, B) | L]) :- ((number(A), number(B)) -> check_complex_list(L); fail).