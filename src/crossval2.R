 
crossval2 <- function(formula1, formula2, data, fold = 10, split_percentage=0.7) {
 
    set.seed(as.numeric(Sys.time()))
 
    data.same <- data[data$actual_vote == data$mvar_1, ]
    data.diff <- data[data$actual_vote != data$mvar_1, ]
 
    index1 <- 1:nrow(data.same)
    index1.train <- sample(index1, split_percentage * nrow(data.same))
    train.same <- data.same[index1.train, ]
    test.same <- data.same[-index1.train, ]
 
    index2 <- 1:nrow(data.diff)
    index2.train <- sample(index2, split_percentage * nrow(data.diff))
    train.diff <- data.diff[index2.train, ]
    test.diff <- data.diff[-index2.train, ]
 
    # Classifier 1
 
    train.clf1 <- rbind(train.same, train.diff)
    test.clf1 <- rbind(test.same, test.diff)
    test.data <- rbind(test.same, test.diff)
    train.clf1$same_as_past <- as.factor(train.clf1$actual_vote == train.clf1$mvar_1)
    test.clf1$same_as_past <- as.factor(test.clf1$actual_vote == test.clf1$mvar_1)
 
    SIZE = 10
    ITERATIONS = 200
 
    fit1 <- nnet(formula1, train.clf1, size=SIZE, maxit=ITERATIONS)
    fcast <- as.factor(predict(fit1, test.clf1, type="class"))
    test.clf1$predicted <- as.factor(fcast)
    check <- (test.clf1$predicted == test.clf1$same_as_past)
    print("Accuracy for first classifier")
    print(100 * sum(check) / length(check))
 
    # Classifier 2
 
    fit2 <- nnet(formula2, train.diff, size=SIZE, maxit=ITERATIONS)
    fcast <- as.factor(predict(fit2, test.diff, type="class"))
    test.diff$predicted <- as.factor(fcast)
    check <- (test.diff$predicted == test.diff$actual_vote)
    print("Accuracy for second classifier")
    print(100 * sum(check) / length(check))
 
    # Total
    test.data$will_be_same <- as.factor(predict(fit1, test.data, type="class"))
    test.data.same <- test.data[test.data$will_be_same == 'TRUE', ]
    test.data.diff <- test.data[test.data$will_be_same == 'FALSE', ]
    test.data.same$predicted <- test.data.same$mvar_1
    check1 <- (test.data.same$predicted == test.data.same$actual_vote)
 
    print(summary(check1))
 
    print ("Same accuracy")
    print(100 * sum(check1) / length(check1))
 
    test.data.diff$predicted <- as.factor(predict(fit2, test.data.diff, type="class"))
    print(summary(test.data.diff$predicted))
    check2 <- (test.data.diff$predicted == test.data.diff$actual_vote)
 
    print(summary(check2))
    print("Different accuracy")
    print(100 * sum(check2) / length(check2))
 
    check <- rbind(check1, check2)
    print("Accuracy for total")
    print(100 * sum(check) / length(check))
 
    score = 0
    max_score = 0
    for (i in 1:nrow(test.data.same)) {
        row <- test.data.same[i, ]
        if (row$actual_vote == row$mvar_1) {
            max_score = max_score + 50
            if (row$actual_vote == row$predicted) {
                score = score + 50
            } else {
                score = score - 100
            }
        } else {
            max_score = max_score + 100
            if (row$actual_vote == row$predicted) {
                score = score + 100
            }
        }
    }
    print("SCORE")
    print(score)
    print(maxscore)
}