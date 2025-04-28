:- set_prolog_flag(encoding, utf8).

% ===== 1. Визначення χ²-функції розподілу (DF = 4) =====
chisq_cdf(X, CDF) :-
    X >= 0,
    CDF is 1 - exp(-X/2) * (1 + X/2).

% ===== 2. Обчислення зворотної функції розподілу (квантилей) =====
chi_square_quantile(P, Q) :-
    quantile_bs(P, 0.0, 100.0, Q).

quantile_bs(P, Low, High, Q) :-
    Mid is (Low + High) / 2,
    chisq_cdf(Mid, CdfMid),
    (   abs(CdfMid - P) < 1e-6
    ->  Q = Mid
    ;   ( CdfMid < P
        ->  quantile_bs(P, Mid, High, Q)
        ;   quantile_bs(P, Low, Mid, Q)
        )
    ).

% ===== 3. Обчислення меж інтервалів (квантілей χ²) =====
compute_boundaries(Series, M, Boundaries) :-
    min_list(Series, Min),                 % мінімум ряду
    max_list(Series, Max),                 % максимум ряду
    chisq_cdf(Min, Pmin),                  % ймовірність для мінімуму
    chisq_cdf(Max, Pmax),                  % ймовірність для максимуму
    findall(
      Q,                                   % знайти всі квантілі
      ( between(0, M, I),
        P is Pmin + I*(Pmax - Pmin)/M,
        chi_square_quantile(P, Q)
      ),
      Boundaries                          % результат — список меж
    ).

% ===== 4. Відображення індексу інтервалу в літеру (1→A,2→B,…) =====
letter_for_index(Index, Letter) :-
    Code is 64 + Index,
    char_code(Letter, Code).

% ===== 5. Мапінг числа в інтервал та літеру =====
numeric_to_letter(Number, Bounds, M, Letter) :-
    numeric_to_letter_aux(Number, Bounds, 1, M, Index),
    letter_for_index(Index, Letter).

numeric_to_letter_aux(N, [B1,B2|_], I, _, I) :-
    N >= B1, N < B2, !.
numeric_to_letter_aux(N, [_|Rest], I, M, Index) :-
    I1 is I + 1,
    numeric_to_letter_aux(N, Rest, I1, M, Index).
numeric_to_letter_aux(N, [B], _, M, M) :-
    N >= B.

% ===== 6. Перетворення всього ряду чисел у послідовність літер =====
transform_series([], _, _, []).
transform_series([H|T], Bounds, M, [L|LT]) :-
    numeric_to_letter(H, Bounds, M, L),
    transform_series(T, Bounds, M, LT).

% ===== 7. Накопичення кількостей переходів між сусідніми літерами =====
build_transitions(Ls, Transitions) :-
    build_transitions_aux(Ls, [], Transitions).

build_transitions_aux([_], Acc, Acc).
build_transitions_aux([X,Y|T], Acc, Out) :-
    update_transition(X, Y, Acc, Acc1),
    build_transitions_aux([Y|T], Acc1, Out).

update_transition(X, Y, Acc, [transition(X,Y,C1)|Rest]) :-
    select(transition(X,Y,C0), Acc, Rest),
    C1 is C0 + 1, !.
update_transition(X, Y, Acc, [transition(X,Y,1)|Acc]).

% ===== 8. Вивід матриці переходів (цілі лічильники) =====
get_count(Trans, F, T, C) :-
    ( member(transition(F,T,C0), Trans) -> C = C0 ; C = 0 ).

print_transition_matrix(Trans, M) :-
    findall(L, (between(1,M,I), letter_for_index(I,L)), Letters),
    nl, write('Transition matrix (counts):'), nl,
    write('    '),
    forall(member(L,Letters),
           format(' ~w', [L])),
    nl,
    forall(member(R,Letters),
      ( write(R), write(':'),
        forall(member(C,Letters),
          ( get_count(Trans,R,C,Count),
            format(' ~w', [Count])    % цілі числа
          )),
        nl
      )
    ).

% ===== 9. Допоміжні предикати для виводу ряду та лінгвістичного рядка =====
print_series([])    :- write('(empty)'), nl.
print_series([H|T]) :-
    write(H),
    ( T = [] -> nl ; write(', '), print_series(T) ).

print_linguistic(Ls) :-
    atomic_list_concat(Ls,'',Str),
    write(Str), nl.

% ===== 10. Автогенерація χ²-ряда для введення «-1» =====
generate_chisq_series(N, Series) :-
    findall(X, (between(1,N,_), random_chisq(4,X)), Series).

random_chisq(4, X) :-
    random(R1), random(R2),
    X is -2*log(R1) + -2*log(R2).

% ===== 11. Головна функція: взаємодія з користувачем та виклик усіх етапів =====
main :-
    write('Enter number of elements (N > 0): '), nl,
    read(N),
    ( integer(N), N > 0 -> true ; write('Invalid N.'), nl, fail ),
    write('Enter alphabet size (M >= 2): '), nl,
    read(M),
    ( integer(M), M >= 2 -> true ; write('Invalid M.'), nl, fail ),
    write('Enter series as list, or -1 for auto (Chi-square): '), nl,
    read(Input),
    ( Input = -1 -> generate_chisq_series(N,Series) ; Series = Input ),
    sort(Series, Sorted),
    nl, write('Sorted series:'), nl,
    print_series(Sorted),
    compute_boundaries(Sorted, M, Bounds),
    nl, write('Boundaries:'), nl,
    print_series(Bounds),
    transform_series(Sorted, Bounds, M, Linguistic),
    nl, write('Linguistic string:'), nl,
    ( Linguistic \= [] -> print_linguistic(Linguistic) ; write('(none)'), nl ),
    build_transitions(Linguistic, Transitions),
    print_transition_matrix(Transitions, M),
    nl, write('Work completed.'), nl.
