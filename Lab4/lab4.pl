:- set_prolog_flag(encoding, utf8).  % встановлюємо кодування UTF-8
:- use_module(library(csv)).       % підключаємо бібліотеку для роботи з CSV-файлами
:- use_module(library(random)).    % підключаємо бібліотеку для генерації випадкових чисел

% ===== 1. Фіксована таблиця χ²-квантилів для df=4 (20 інтервалів + Inf) =====
chi_table([0.0,0.71,1.06,1.37,1.65,1.92,2.19,2.47,2.75,3.05,
           3.36,3.69,4.04,4.44,4.88,5.39,5.99,6.74,7.78,9.49,inf]).

% ===== 2. Вивід числової послідовності =====
print_series([]) :- nl.                     % порожня послідовність → перехід на новий рядок
print_series([X]) :- format('~w~n', [X]).    % одиночний елемент → вивід з новим рядком
print_series([X,Y|T]) :-                    % два й більше елементи → вивід через кому
    format('~w, ', [X]),
    print_series([Y|T]).

% ===== 3. Вивід меж інтервалів =====
print_boundaries(Bounds) :-
    length(Bounds, L),
    M is L - 1,                            % кількість реальних інтервалів = довжина списку - 1
    nl, write('Interval boundaries:'), nl,
    forall(between(1, M, I),
      ( nth1(I, Bounds, B1),               % нижня межа
        I1 is I + 1,
        nth1(I1, Bounds, B2),              % верхня межа
        ( B1 == inf -> B1r = 'Inf' ; B1r is round(B1*100)/100 ),
        ( B2 == inf -> B2r = 'Inf' ; B2r is round(B2*100)/100 ),
        format('Interval ~d: [~w, ~w]~n', [I, B1r, B2r])
      )).

% ===== 4. Визначення літери за індексом =====
letter_for_index(I, L) :-
    Code is 64 + I,                      % A -> код 65, тому додаємо I до 64
    char_code(L, Code).

% ===== 5. Відображення числа на інтервал через χ²-таблицю та нормалізацію =====
numeric_to_letter(X, ChiTab, M, MinV, MaxV, Letter) :-
    % нормалізація значення у діапазон [0.0001,0.9999]
    P0 is (X - MinV) / (MaxV - MinV),
    P  is max(0.0001, min(0.9999, P0)),
    % обчислення індексу квантиля
    IdxF is P * M,
    Idx  is ceiling(IdxF),
    nth1(Idx, ChiTab, Q),                  % знаходимо відповідний квантиль
    % формуємо список інтервалів (нижня, верхня межі χ²)
    findall((I, Lb, Ub),
        ( between(1, M, I), nth1(I, ChiTab, Lb), I2 is I+1, nth1(I2, ChiTab, Ub) ),
        Intervals),
    member((J, Low, High), Intervals),     % знаходимо, куди потрапляє Q
    Q >= Low, Q < High,
    !,                                     % перестаємо шукати далі
    letter_for_index(J, Letter),          % перетворюємо індекс на літеру
    format('LOG: ~w -> chi2(~w) -> interval ~d -> ~w~n', [X, Q, J, Letter]).
numeric_to_letter(X, ChiTab, M, _, _, Letter) :-
    % catch-all для значень >= останньої межі
    nth1(M, ChiTab, Low),
    X >= Low,
    letter_for_index(M, Letter),
    format('LOG: ~w >= ~w -> interval ~d -> ~w~n', [X, Low, M, Letter]).

% ===== 6. Перетворення всієї числової послідовності =====
transform_series([], _, _, _, _, []).
transform_series([H|T], ChiTab, M, MinV, MaxV, [L|Ls]) :-
    numeric_to_letter(H, ChiTab, M, MinV, MaxV, L),
    transform_series(T, ChiTab, M, MinV, MaxV, Ls).

% ===== 7. Побудова матриці переходів =====
build_transitions([_], Acc, Acc) :- !.
build_transitions([A,B|T], Acc, Out) :-
    ( select(transition(A,B,C0), Acc, Rest)
      -> C1 is C0 + 1, Acc1 = [transition(A,B,C1)|Rest]
      ;  Acc1 = [transition(A,B,1)|Acc]
    ),
    build_transitions([B|T], Acc1, Out).
build_transitions(Ltrs, Trans) :-
    build_transitions(Ltrs, [], Trans).
get_count(Trans, F, T, C) :-
    ( member(transition(F,T,C0), Trans) -> C = C0 ; C = 0 ).

% ===== 8. Вивід матриці передування =====
print_transition_matrix(Trans, M) :-
    findall(L, (between(1, M, I), letter_for_index(I, L)), Letters),
    nl, write('Transition matrix:'), nl,
    write('    '), forall(member(L, Letters), format(' ~w ', [L])), nl,
    forall(member(R, Letters), (
      format('~w:', [R]),
      forall(member(C, Letters), (get_count(Trans, R, C, Cn), format(' ~|~t~d~3+', [Cn]))), nl
    )).

% ===== 9. Генерація випадкової послідовності =====
generate_random_series(N, Series) :-
    Max is 20,                         % максимальне значення для генерації
    findall(X, (between(1, N, _), random(0, Max, R), X is round(R*100)/100), Series).

% ===== 10. Головний потік програми =====
main :-
    write('Read from CSV? (y/n): '), read(Mode),
    ( Mode == y -> file_flow ; manual_flow ).

% файлова гілка: зчитуємо дані з data.csv
file_flow :-
    write('Reading data.csv...'), nl,
    write('How many elements? '), read(N),
    write('Alphabet size M? '), read(M),
    csv_read_file('data.csv', [H|Rows], [functor(row), convert(true)]),
    H =.. [_|Cols], nth1(ColI, Cols, 'Price'),
    length(Pref, N), append(Pref, _, Rows),
    findall(X, (member(R, Pref), R =.. [_|Vs], nth1(ColI, Vs, X)), Series),
    process_data(Series, M).

% інтерактивна гілка: генерація чи введення вручну
manual_flow :-
    write('Enter number of elements (N): '), read(N),
    write('Enter \u0027-1\u0027 for auto-generation or \u00270\u0027 for manual input: '), read(InputMode),
    (
        InputMode == -1 ->
            write('Enter alphabet size (M >= 2): '), read(M),
            generate_random_series(N, Series)
    ;   InputMode == 0 ->
            write('Enter series as list: '), read(Series),
            write('Enter alphabet size (M >= 2): '), read(M)
    ;   write('Invalid input mode.'), nl, fail
    ),
    process_data(Series, M).

% обробка даних: сортування, вивід, трансформація, матриця
process_data(Series, M) :-
    msort(Series, Sorted),                 % сортуємо значення
    Sorted = [MinV|_], last(Sorted, MaxV),  % визначаємо мінімум і максимум
    nl, write('Sorted numerical series:'), nl, print_series(Sorted),
    chi_table(ChiTab), print_boundaries(ChiTab),
    transform_series(Sorted, ChiTab, M, MinV, MaxV, Ling),
    nl, write('Linguistic series:'), nl, atomic_list_concat(Ling, '', Str), write(Str), nl,
    msort(Ling, SortedLing),               % сортуємо літери для виводу окремого ряду
    nl, write('Sorted linguistic series:'), nl, atomic_list_concat(SortedLing, '', SLStr), write(SLStr), nl,
    build_transitions(Ling, Trans), print_transition_matrix(Trans, M),
    nl, write('Done.'), nl.

:- initialization(main).
