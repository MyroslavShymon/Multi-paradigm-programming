# ЛАБОРАТОРНА РОБОТА №3 (Варіант 4: Χі-розподіл)

# 1. Параметри розподілу(Число ступенів свободи для Χі-розподілу)
# ----------------------
df <- 4

# 2. Генерація або зчитування вхідного числового ряду
# ---------------------------------------------------
# 2.1. Генерує n значень за Χі-розподілом або зчитує їх з консолі
generate_series <- function(n, auto_gen = TRUE) {
  if (auto_gen) {
    # 2.1.1. Автогенерація: rchisq
    series <- rchisq(n, df = df)
  } else {
    # 2.1.2. Ручне введення: рядок пробілів → вектор чисел
    input_line <- readline(prompt = "Введіть числа, розділені пробілами: ")
    series <- as.numeric(unlist(strsplit(input_line, "\\s+")))
    if (length(series) != n) {
      stop("Кількість введених чисел не відповідає зазначеному n.")
    }
  }
  return(series)
}


# 3. Обчислення меж інтервалів за Χі-розподілом
# --------------------------------------------
# 3.1. Визначаємо функції розподілу та обернену функцію розподілу (pchisq / qchisq)
# 3.2. Розбиваємо ймовірнісний інтервал [P(min), P(max)] на m частин
# 3.3. За квантілями повертаємо границі
calc_boundaries <- function(series, m) {
  series_min <- min(series)          # 3.4. Мінімум
  series_max <- max(series)          # 3.5. Максимум
  p_lower     <- pchisq(series_min, df = df)
  p_upper     <- pchisq(series_max, df = df)
  p_bounds    <- seq(p_lower, p_upper, length.out = m + 1)
  boundaries  <- qchisq(p_bounds, df = df)
  return(boundaries)
}


# 4. Перетворення чисел у символи алфавіту
# ----------------------------------------
# 4.1. Для кожного x визначаємо індекс інтервалу через findInterval
# 4.2. Корегуємо вихід за межами [1,m]
# 4.3. Повертаємо вектор символів
numeric_to_letters <- function(series, boundaries, letters) {
  indices <- findInterval(series, boundaries, rightmost.closed = TRUE)
  indices[indices < 1]             <- 1
  indices[indices > length(letters)] <- length(letters)
  linguistic <- letters[indices]
  return(linguistic)
}


# 5. Побудова матриці переходів (матриці передування)
# ---------------------------------------------------
# 5.1. Ініціалізація матриці нулями m×m
# 5.2. Для кожного кроку i→i+1 інкрементуємо відповідну комірку
build_transition_matrix <- function(linguistic, m) {
  trans_matrix <- matrix(0, nrow = m, ncol = m)
  for (i in seq_len(length(linguistic) - 1)) {
    from    <- linguistic[i]
    to      <- linguistic[i + 1]
    row_idx <- match(from, LETTERS)
    col_idx <- match(to,   LETTERS)
    if (!is.na(row_idx) && !is.na(col_idx)) {
      trans_matrix[row_idx, col_idx] <- trans_matrix[row_idx, col_idx] + 1
    }
  }
  return(trans_matrix)
}


# 6. Головна функція: зчитування, обробка, вивід
# ---------------------------------------------
main <- function() {
  # 6.1. Зчитуємо N та перевіряємо
  n <- as.integer(readline(prompt = "Введіть кількість елементів чисельного ряду (N): "))
  if (is.na(n) || n <= 0) stop("Некоректне значення N.")
  
  # 6.2. Зчитуємо m (розмір алфавіту) та перевіряємо
  m <- as.integer(readline(prompt = "Введіть розмір алфавіту (m, мінімум 2): "))
  if (is.na(m) || m < 2) stop("Некоректний розмір алфавіту.")
  
  # 6.3. Визначаємо, чи автогенерувати ряд
  auto_input <- readline(prompt = "Введіть '-1' для автогенерації або '0' для ручного введення: ")
  auto_gen    <- identical(auto_input, "-1")
  
  # 6.4. Генеруємо або зчитуємо ряд та сортуючи
  series      <- sort(generate_series(n, auto_gen))
  
  # 6.5. Обчислюємо межі інтервалів
  boundaries  <- calc_boundaries(series, m)
  
  # 6.6. Формуємо вектор перших m літер латинського алфавіту
  letters_used <- LETTERS[1:m]
  
  # 6.7. Перетворюємо числовий ряд у літерний
  linguistic   <- numeric_to_letters(series, boundaries, letters_used)
  
  # 6.8. Будуємо матрицю передування
  trans_matrix <- build_transition_matrix(linguistic, m)
  
  # 6.9. Виводимо результати
  cat("\nВідсортований числовий ряд:\n")
  cat(sprintf("%.2f", series), sep = ", ", "\n")
  
  cat("\nЛінгвістичний ряд (послідовність символів):\n")
  cat(paste(linguistic, collapse = ""), "\n")
  
  cat("\nМатриця передування (частота переходів):\n")
  cat(sprintf("%6s", ""), paste(sprintf("%4s", letters_used), collapse = ""), "\n")
  for (i in seq_len(m)) {
    cat(sprintf("%4s: ", letters_used[i]), paste(sprintf("%4d", trans_matrix[i, ]), collapse = ""), "\n")
  }
  
  cat("\nРоботу завершено.\n")
}

# 7. Виклик головної функції
# --------------------------
main()
