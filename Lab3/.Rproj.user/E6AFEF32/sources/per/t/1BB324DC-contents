# ЛАБОРАТОРНА РОБОТА №3 (Варіант 4: Χі-розподіл)
# ----------------------------------------------

# 1. Параметри розподілу
df <- 4

# 2. Генерація або зчитування числового ряду
generate_series <- function(n, auto_gen = TRUE) {
  if (auto_gen) {
    series <- rchisq(n, df = df)
  } else {
    input_line <- readline(prompt = "Введіть числа, розділені пробілами: ")
    series <- as.numeric(unlist(strsplit(input_line, "\\s+")))
    if (length(series) != n) {
      stop("Кількість введених чисел не відповідає зазначеному n.")
    }
  }
  return(series)
}

# 3. Обчислення меж інтервалів через χ²-квантілі
calc_boundaries <- function(series, m) {
  # Нормалізований ряд до [0, 1]
  normalized <- (series - min(series)) / (max(series) - min(series))
  
  # Ймовірності на межах інтервалів (вже в нормалізованому просторі)
  p_bounds <- seq(0, 1, length.out = m + 1)
  
  # Масштабуємо межі інтервалів через qchisq
  boundaries <- qchisq(p_bounds, df = df)
  
  return(boundaries)
}


# 4. Перетворення чисел у літери
numeric_to_letters <- function(series, boundaries, letters) {
  indices <- findInterval(series, boundaries, rightmost.closed = TRUE)
  indices[indices < 1] <- 1
  indices[indices > length(letters)] <- length(letters)
  linguistic <- letters[indices]
  return(linguistic)
}

# 5. Побудова матриці передування
build_transition_matrix <- function(linguistic, m) {
  trans_matrix <- matrix(0, nrow = m, ncol = m)
  for (i in seq_len(length(linguistic) - 1)) {
    from <- linguistic[i]
    to <- linguistic[i + 1]
    row_idx <- match(from, LETTERS)
    col_idx <- match(to, LETTERS)
    if (!is.na(row_idx) && !is.na(col_idx)) {
      trans_matrix[row_idx, col_idx] <- trans_matrix[row_idx, col_idx] + 1
    }
  }
  return(trans_matrix)
}

# 6. Основна функція
main <- function() {
  use_file <- tolower(readline("Читати дані з файлу data.csv? (Y/N): ")) == "y"
  
  if (use_file) {
    if (!file.exists("data.csv")) stop("Файл data.csv не знайдено.")
    data <- read.csv("data.csv", header = TRUE)
    if (!"Price" %in% names(data)) stop("Колонка 'Price' відсутня у файлі.")
    available <- length(data$Price)
    cat(sprintf("У файлі доступно %d значень у колонці Price.\n", available))
    n <- as.integer(readline("Скільки значень зчитати з файлу?: "))
    if (is.na(n) || n <= 0 || n > available) stop("Некоректна кількість значень.")
    series <- head(as.numeric(data$Price), n)
  } else {
    n <- as.integer(readline("Введіть кількість елементів чисельного ряду (N): "))
    if (is.na(n) || n <= 0) stop("Некоректне значення N.")
    auto_input <- readline("Введіть '-1' для автогенерації або '0' для ручного введення: ")
    auto_gen <- identical(auto_input, "-1")
    series <- generate_series(n, auto_gen)
  }
  
  series <- sort(series)
  
  m <- as.integer(readline("Введіть розмір алфавіту (m, мінімум 2): "))
  if (is.na(m) || m < 2 || m > length(series)) stop("Некоректний розмір алфавіту.")
  letters_used <- LETTERS[1:m]
  
  boundaries <- calc_boundaries(series, m)
  
  # Масштабуємо series у простір χ²
  normalized <- (series - min(series)) / (max(series) - min(series))
  series_chi <- qchisq(normalized, df = df)
  linguistic <- numeric_to_letters(series_chi, boundaries, letters_used)
  matrix <- build_transition_matrix(linguistic, m)
  
  cat("\nВідсортований числовий ряд:\n")
  cat(sprintf("%.2f", series), sep = ", ", "\n")
  
  cat("\nМежі інтервалів:\n")
  for (i in 1:m) {
    cat(sprintf("Інтервал %d: [%.2f, %.2f]\n", i, boundaries[i], boundaries[i + 1]))
  }
  
  cat("\nЛінгвістичний ряд:\n")
  cat(paste(linguistic, collapse = ""), "\n")
  
  cat("\nМатриця передування:\n")
  cat(sprintf("%6s", ""), paste(sprintf("%4s", letters_used), collapse = ""), "\n")
  for (i in seq_len(m)) {
    cat(sprintf("%4s: ", letters_used[i]), paste(sprintf("%4d", matrix[i, ]), collapse = ""), "\n")
  }
  
  cat("\nРоботу завершено.\n")
}

# 7. Запуск
main()
