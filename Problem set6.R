library(readxl)
setwd("Z:/")
rm(list = ls())

data <- read.csv("nLx_values.csv")
data_b <- data[1:50, ]
data_w <- data[51:100, ]

# black population (survival rate)
for (age_group in seq(5, 40, by = 5)) {
  matrix_name <- paste0("b_S", age_group)
  assign(matrix_name, matrix(0, nrow = 5, ncol = 5), envir = .GlobalEnv)
  for (educ in 1:5) {
    nLX_1 <- (educ - 1) * 10 + age_group / 5 + 1
    nLX_2 <- nLX_1 + 1
    if (nLX_2 <= nrow(data_b)) {
      survival_rate <- data_b$nLx[nLX_2] / data_b$nLx[nLX_1]
      current_matrix <- get(matrix_name, envir = .GlobalEnv)
      current_matrix[educ, educ] <- survival_rate
      assign(matrix_name, current_matrix, envir = .GlobalEnv)
    }
  }
}

# white population (survival rate)
for (age_group in seq(5, 40, by = 5)) {
  matrix_name <- paste0("w_S", age_group)
  assign(matrix_name, matrix(0, nrow = 5, ncol = 5), envir = .GlobalEnv)
  for (educ in 1:5) {
    nLX_1 <- (educ - 1) * 10 + age_group / 5 + 1
    nLX_2 <- nLX_1 + 1
    if (nLX_2 <= nrow(data_w)) {
      survival_rate <- data_w$nLx[nLX_2] / data_w$nLx[nLX_1]
      current_matrix <- get(matrix_name, envir = .GlobalEnv)
      current_matrix[educ, educ] <- survival_rate
      assign(matrix_name, current_matrix, envir = .GlobalEnv)
    }
  }
}

# Intergenerational Mobility (Mij)

b_m <- matrix(c(0.132, 0.034, 0.014, 0.01, 0.001, 
                   0.179, 0.15, 0.061, 0.027, 0.033,
                   0.485, 0.427, 0.457, 0.243, 0.143, 
                   0.13, 0.208, 0.25, 0.338, 0.259, 
                   0.075, 0.18, 0.217, 0.381, 0.564), nrow = 5, ncol = 5)

w_m <- matrix(c(0.289, 0.079, 0.025, 0.033, 0, 
                   0.268, 0.35, 0.19, 0.038, 0.032, 
                   0.243, 0.278, 0.386, 0.243, 0.163, 
                   0.126, 0.164, 0.212, 0.496, 0.371, 
                   0.073, 0.129, 0.188, 0.189, 0.434), nrow = 5, ncol = 5)

# Fertility rate (From Mare) Fiat matrix

b_f <- read.csv("b_f.csv") * (5/1000)
w_f <- read.csv("w_f.csv") * (5/1000)

# Siat matrix 

BS_b <- matrix(NA, nrow = 5, ncol = 6)

for (educ in 1:5) {
  for (age_col in 1:6) {
    age_current = 3 + age_col + (educ - 1) * 10
    age_next = age_current + 1
    if (age_next <= nrow(data_b)) {
      BS_b[educ, age_col] <- data_b$nLx[age_next] / data_b$nLx[age_current]
    }
  }
}

BS_w <- matrix(NA, nrow = 5, ncol = 6)

for (educ in 1:5) {
  for (age_col in 1:6) {
    age_current = 3 + age_col + (educ - 1) * 10
    age_next = age_current + 1
    if (age_next <= nrow(data_w)) {
      BS_w[educ, age_col] <- data_w$nLx[age_next] / data_w$nLx[age_current]
    }
  }
}

# Liot 
b_L0 <- matrix(NA, nrow = 5, ncol = 1)

for (educ in 1:5) {
  index <- (educ - 1) * 10 + 1
    b_L0[educ, 1] <- data_b$nLx[index] / 200000
}


w_L0 <- matrix(NA, nrow = 5, ncol = 1)

for (educ in 1:5) {
  index <- (educ - 1) * 10 + 1
  w_L0[educ, 1] <- data_w$nLx[index] / 200000
}

# calculate B matrix 

b_B0 <- matrix(NA, nrow = 5, ncol = 5)

for (i in 1:5) {
  for (j in 1:5) {
    b_f_component <- b_f[i, 1] + BS_b[i, 1] * b_f[i, j + 1]
    b_B0[i, j] <- b_L0[i, 1] * b_f_component * b_m[i, j]
  }
}




