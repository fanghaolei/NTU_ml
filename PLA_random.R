data <- read.table("https://d396qusza40orc.cloudfront.net/ntumlone%2Fhw1%2Fhw1_15_train.dat")

# sign function, 0 is marked as -1
sign <- function(x){
    if (x > 0) x = 1
    if (x <= 0) x= -1
    return(x)
}

# in R: w: 5x1 vec, X[i, ]: 5x1 vec, Y[i]: int
# t(w): 1x5 vec, t(w)%*%X[i, ]: 1x1 mat
# Y[i]%*%X[i, ]: 1x5 vector


# a vector to store num of iterations
update <- vector("numeric", length = 2000L)

# iterate PLA 2000 times
for(j in 1:2000){
    # set a random seed each time and construct data
    set.seed(sample(1:10000, 1, replace = F))
    data <- data[sample(1:400), ]
    X <- cbind(rep(1, nrow(data)), as.matrix(data[, c(1:4)])) 
    y <- data[, 5]
   
    # set initial weight w, a 5x1 vector
    w <- rep(0, 5)
    eta <- .5
    count <- 0
    # create a vector to store results
    n_corr <- vector("numeric", length = nrow(data))
    while (sum(n_corr) < 400) {
        for(i in 1:nrow(data)){
            score <- X[i, ]%*%w # score is an integer
            if (sign(score) == y[i]) {
                n_corr[i] <- 1
                # do not update if sign(score) == y
                next
            } else {
                n_corr[i] <- 0
                count <- count + 1
                # update w if sign(score) != y
                # if sign(score) > y = -1, shrink w to make it closer to -1; 
                # if sign(socre) < y = 1, enlarge w to make it closer to 1.
                w <- w + eta*y[i]*X[i, ]
            }
        }
    }
    update[j] <- count
    print(paste("iteration", j, "updates", update[j]))
}

