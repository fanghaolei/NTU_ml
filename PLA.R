data <- read.table("https://d396qusza40orc.cloudfront.net/ntumlone%2Fhw1%2Fhw1_15_train.dat")

# bind a x_0 vector as threshold
X <- cbind(rep(1, nrow(data)), as.matrix(data[, c(1:4)])) 

# y vector
y <- data[, 5]

# create a sign function, 0 is marked as -1
sign <- function(x){
    if (x > 0) x = 1
    if (x <= 0) x= -1
    return(x)
}

# in R:
# w: 5x1 vec, X[i, ]: 5x1 vec, Y[i]: int
# t(w): 1x5 vec, t(w)%*%X[i, ]: 1x1 mat
# Y[i]%*%X[i, ]: 1x5 vector

# set initial weight w, a 5x1 vector
w <- rep(0, 5)

# create a vector to store results
n_correct <- vector("numeric", length = nrow(data))
update = 0

while (sum(n_correct) < 400) {
    for(i in 1:nrow(data)){
        score <- X[i, ]%*%w # score is an integer
        if (sign(score) == y[i]) {
            print(paste(i, "is correct! moving on..."))
            n_correct[i] <- 1
            
            # do not update if sign(score) == y
            next
        } else {
            print(paste(i, "is wrong, correcting mistake..."))
            n_correct[i] <- 0
            update <- update + 1
            # update w if sign(score) != y
            # if sign(score) > y = -1, shrink w to make it closer to -1; 
            # if sign(socre) < y = 1, enlarge w to make it closer to 1.
            w <- w + y[i]*X[i, ]
            print(paste("current update", update))
        }
    }
}