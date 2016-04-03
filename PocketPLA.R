train <- read.table('https://d396qusza40orc.cloudfront.net/ntumlone%2Fhw1%2Fhw1_18_train.dat')
test <- read.table('https://d396qusza40orc.cloudfront.net/ntumlone%2Fhw1%2Fhw1_18_test.dat ')

# sign function, 0 is marked as -1
sign <- function(x){
    if (x > 0) x = 1
    if (x <= 0) x= -1
    return(x)
}

# funtion to calculate error rate using current w
cal_error <- function(w, data){
    m <- nrow(data)
    X <- cbind(rep(1, m), as.matrix(data[1:4]))
    y <- data[, 5]
    pred <- sapply(X%*%w, sign)
    sum(pred != y)/m
}

updates <- 100
niter <- 2000
test_error <- vector("numeric", length = niter)

# train with pocket algorithm
# very slow for 2000 iterations, don't know how to improve yet
for (j in 1:niter) {
    # for each iteration, visit training examples randomly
    train <- train[sample(1: m), ]
    m <- nrow(train)
    train_X <- cbind(rep(1, m), as.matrix(train[1:4]))
    train_y <- train[, 5]
    
    # initialize the loop with random w
    w <- list()
    w[[1]] <- runif(5, 0, 1)
    
    # count update times
    update <- 0
    train_error <- numeric()
    train_error[1] <- cal_error(w[[1]], train)
    # loop pla until 50 updates on training set 
    for (i in 1:nrow(train)){
        if (update < updates) {
            score <- train_X[i, ]%*%w[[i]] # score is an integer
            if (sign(score) == train_y[i]) {
                # do not update if sign(score) == y
                w[[i+1]] <- w[[i]]
                train_error[i+1] <- cal_error(w[[i+1]], train)
                # uncomment for debugging
                # print(paste(i, "is correct, skipped. training error:"
                #            , train_error[i]))
                next
            } else {
                #print(paste(i, "is wrong, updating...", update))
                # update w if sign(score) != y
                update <- update + 1
                w[[i+1]] <- w[[i]] + train_y[i]*train_X[i, ]
                # uncomment for debugging
                train_error[i+1] <- cal_error(w[[i+1]], train)
                # print(paste(i, "is wrong, updated. training error:"
                #           , train_error[i]))
            }
        } else break
    }
    # keep the w with lowest training error
    pocket_w <- w[[which.min(train_error)]] 
    # apply pocket_w to test set
    print(paste("applying", j, "pocket w prediction..."))
    test_error[j] <- cal_error(pocket_w, test)
    print(paste("test error with pocket w: ", test_error[j]))
}

# Q18, mean test error rate
mean(test_error)


