library(expm)

mat <- matrix(
    c(3/5, 1/10, 3/4, 0, 3/10, 2/5, 0, 0, 0, 1/2, 1/5, 0, 1/10, 0, 1/20, 1),
    nrow = 4,
    ncol = 4,
    byrow = TRUE)

matrixPower <- function(P, n)
{
    matrix <- P %^% n
    return(matrix)
}

matrixTransition <- function(P, n, x) {
    # P matrice de transition
    # n temps
    # x current state

    trajectory <- c(x) # x here is the first state at n = 0

    for (time in 1:(n + 1)) {

        random <- runif(1, 0, 1) # random number to simulate the probability
        probabilities <- c()
        count <- 0

        for (i in 1:(nrow(P))) { # select probablities for each possible next state
            probabilities <- c(probabilities, P[i, x])
        }

        result <- 1
        for (probability in probabilities) {

            if ((random > count) && ( random < probability + count )) {
                trajectory <- append(trajectory, result)
                x <- result
            }

            count <- count + probability
            result <- result + 1
        }
    }

    return(trajectory)
}

barDiagram <- function(trajectory) {
    barplot(table(trajectory))
}

averageLoopTime <- function(trajectory) {
    time <- 0
    count <- 0
    x <- 0
    initial <- trajectory[1]

    for (i in trajectory) {
        count <- count + 1
        if (initial == i) {
            time <- time + count
            x = x + 1
            count <- 0
        }
    }

    return (time / x)
}

print(mat)

matTrajectory <- matrixTransition(mat, 50, 1)

print(matTrajectory)

barDiagram(matTrajectory)

print(averageLoopTime(matTrajectory))
