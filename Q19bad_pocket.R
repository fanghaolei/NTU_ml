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

updates <- 50
niter <- 2000
test_error <- vector("numeric", length = niter)

# train with modified pocket algorithm using last-updated w
for (j in 1:niter) {
    # for each iteration, visit training examples randomly
    train <- train[sample(1: m), ]
    m <- nrow(train)
    train_X <- cbind(rep(1, m), as.matrix(train[1:4]))
    train_y <- train[, 5]
    
    # initialize the loop with random w
    w <- runif(5, 0, 1)
    # count update times
    update <- 0
    # loop pla until 50 updates on training set
    for (i in 1:nrow(train)){
        if (update < updates) {
            score <- train_X[i, ]%*%w # score is an integer
            if (sign(score) == train_y[i]) {
                # do not update if sign(score) == y
                # uncomment for debugging
                # print(paste(i, "is correct")
                next
            } else {
                #print(paste(i, "is wrong, updating...", update))
                # update w if sign(score) != y
                update <- update + 1
                w <- w+ train_y[i]*train_X[i, ]
                # uncomment for debugging
                # print(paste(i, "is wrong, updated")
            }
        } else break
    }
    # apply last updated w to test set
    print(paste("applying", j, "last updated w prediction..."))
    test_error[j] <- cal_error(w, test)
    print(paste("test error with last updated w: ", test_error[j]))
}

# Q19, mean test error rate
mean(test_error)


