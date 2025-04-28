program ChiSquaredTransform
    implicit none

    ! 1. Ініціалізація констант та оголошення змінних
    integer, parameter :: maxN = 100       ! Максимальна кількість елементів
    integer, parameter :: maxAlphabet = 26 ! Максимальна потужність алфавіту

    real*8, dimension(maxN) :: data        ! Масив даних (серія чисел)
    real*8 :: minVal, maxVal               ! Мінімальне та максимальне значення
    real*8, dimension(maxAlphabet+1) :: bounds  ! Межі інтервалів
    integer, dimension(maxAlphabet, maxAlphabet) :: matrix ! Матриця переходів

    integer :: N            ! Кількість елементів у серії
    integer :: m            ! Потужність алфавіту (кількість символів)
    integer :: i, j, k      ! Лічильники у циклах
    integer :: idx1, idx2   ! Індекси для матриці переходів

    character(len=1), dimension(maxAlphabet) :: alphabet ! Масив символів алфавіту
    character(len=1), dimension(maxN) :: resultChars     ! Результуючий ряд символів

    ! 2. Заповнення масиву символів латинського алфавіту
    do i = 1, maxAlphabet
        alphabet(i) = char(64 + i)
    end do

    ! 3. Зчитування розміру серії та перевірка коректності
    print *, "Enter the number of elements N (<= ", maxN, "):"
    read *, N
    if (N < 1 .or. N > maxN) then
        print *, "Invalid N, terminating program."
        stop
    end if

    ! 4. Зчитування розміру алфавіту та перевірка коректності
    print *, "Enter the alphabet size m (<= ", maxAlphabet, "):"
    read *, m
    if (m < 2 .or. m > maxAlphabet) then
        print *, "Invalid m, terminating program."
        stop
    end if

    ! 5. Зчитування або генерація числової серії
    print *, "Enter ", N, " numbers (or -1 for auto-generation of a Chi-Square series):"
    read *, data(1)
    if (data(1) == -1.0d0) then
        call random_seed()
        do i = 1, N
            data(i) = 0.0d0
            do j = 1, m
                call random_number(minVal)
                data(i) = data(i) + (-2.0d0 * log(minVal))
            end do
        end do
    else
        do i = 2, N
            read *, data(i)
        end do
    end if

    ! 6. Сортування серії чисел у порядку зростання (бульбашковий алгоритм)
    do i = 1, N-1
        do j = i+1, N
            if (data(j) < data(i)) then
                minVal = data(i)
                data(i) = data(j)
                data(j) = minVal
            end if
        end do
    end do

    ! 7. Визначення мінімального та максимального значень у відсортованій серії
    minVal = data(1)
    maxVal = data(N)

    ! 8. Розбиття інтервалу [minVal, maxVal] на m рівних частин
    bounds(1) = minVal
    bounds(m+1) = maxVal
    do k = 1, m-1
        bounds(k+1) = minVal + (maxVal - minVal) * k / m
    end do

    ! 9. Преобразування чисел у відповідні символи алфавіту
    do i = 1, N
        do k = 1, m
            if (data(i) <= bounds(k+1)) then
                resultChars(i) = alphabet(k)
                exit
            end if
        end do
    end do

    ! 10. Ініціалізація матриці переходів нулями
    matrix = 0

    ! 11. Заповнення матриці переходів частотами послідовних символів
    do i = 1, N-1
        idx1 = ichar(resultChars(i)) - 64
        idx2 = ichar(resultChars(i+1)) - 64
        if (idx1 >= 1 .and. idx1 <= m .and. idx2 >= 1 .and. idx2 <= m) then
            matrix(idx1, idx2) = matrix(idx1, idx2) + 1
        end if
    end do

    ! 12. Вивід відсортованої серії чисел
    print *, "Sorted numerical series:"
    do i = 1, N
        if (i < N) then
            write(*,'(F7.2, ", ")', advance="no") data(i)
        else
            write(*,'(F7.2)') data(i)
        end if
    end do
    print *

    ! 13. Вивід лінгвістичного рядка
    print *, "Linguistic string (sequence of symbols):"
    write(*,'(100(A,1X))') resultChars(1:N)

    ! 14. Вивід матриці переходів
    print *, "Transition matrix (frequency of transitions):"
    write(*,'(4X, 100(A,1X))') (alphabet(k), k=1, m)
    do i = 1, m
        write(*,'(A,3X,100(I3,1X))') alphabet(i), (matrix(i,j), j=1, m)
    end do

end program ChiSquaredTransform
