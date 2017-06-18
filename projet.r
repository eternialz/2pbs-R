library(expm)

mat <- matrix(
    c(0, 3/10, 1/2, 7/10, 2/5, 1/2, 3/10, 3/10, 0),
    nrow = 3,
    ncol = 3,
    byrow = TRUE)

theoDataValues <- c(45/169, 85/169, 39/169)
data1 <- c(266461, 502617, 230924)
data2 <- c(266261, 503079, 230662)
data3 <- c(266461, 502275, 231275)
# manger = 1
# dormir = 2
# jouer = 3

#       (0.0, 0.7, 0.3)
#   P = (0.3, 0.4, 0.3)
#       (0.5, 0.5, 0.0)

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
                trajectory[time + 1] <- result
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
    index <- 0
    initial <- trajectory[1]

    for (i in trajectory) {
        if ((initial == i) && ( index != 0 )) {
            time <- time + count
            x = x + 1
            count <- 0
        }

        count <- count + 1
        index <- index + 1
    }

    return (time / x)
}

khiTerm <- function(obsValue, theoValue) {
    #obsValue and theoValue are numbers
    return(((obsValue - theoValue) * (obsValue - theoValue)) / theoValue)
}

khiTest <- function(obsData, theoValues) {
    #obsData and theoData are table
    #Here degree of freedom is 2 and we choose alpha = 0.05
    obsValues <- as.vector(obsData)
    #obsValues <- obsValues/sum(obsValues)
    theoValues <- theoValues*sum(obsValues)
    khiDeux <- 0

    for(i in 1:(length(obsValues))){
        khiDeux <- khiDeux + khiTerm(obsValues[i], theoValues[i])
    }
    if (khiDeux < qchisq(0.95, 2)) {
        return(TRUE)
    }

    return(FALSE)
}

simulateTraj <- function(P, times, state, simTimes, theoValues) {
    results <- c()
    for(i in 1:times){
      traj <- table(matrixTransition(P, simTimes, state))
      results[i] <- khiTest(traj, theoValues)
    }
    return(table(results))
}

print(mat)

##Test for 3.2.5
print(khiTest(data1, theoDataValues))
print(khiTest(data2, theoDataValues))
print(khiTest(data3, theoDataValues))

print(simulateTraj(mat, 1000, 1, 50, theoDataValues))
print(simulateTraj(mat, 1000, 2, 50, theoDataValues))
print(simulateTraj(mat, 1000, 3, 50, theoDataValues))
#matTrajectory <- matrixTransition(mat, 1000000, 3)

#print(table(matTrajectory))

#barDiagram(matTrajectory)

#print(averageLoopTime(matTrajectory))
mat50 <- matrixPower(mat, 50)
print(simulateTraj(mat, 1000, 1, 50, mat50[,1]))
print(simulateTraj(mat, 1000, 2, 50, mat50[,2]))
print(simulateTraj(mat, 1000, 3, 50, mat50[,3]))
#print(mat50)
