library(expm)

mat <- matrix(c(5, 1, 4, 2), nrow=2)

MatrixPower <- function(P, n){
  matrix <- P %^% n
  return(matrix)
}

print(MatrixPower(mat, 4))