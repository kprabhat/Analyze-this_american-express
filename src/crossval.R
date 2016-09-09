crossval <- function(form, x, form2 = actual_vote ~ ., fold = 10, cp = 0.01, model='nnet') {
    n <- nrow(x)
    prop <- n %/% fold
    set.seed(7)
    newseq <- rank(runif(n))
    k <- as.factor((newseq - 1)%/%prop + 1)
    y <- unlist(strsplit(as.character(form), " "))[2]
    vec.accuracy <- vector(length = fold)

    for (i in seq(fold)) {
        
        if(model == 'rpart'){
            # Decision Tree
            fit <- rpart(form, data = x[k != i,], method = "class")
            # print(fit)
            fcast <- predict(fit, newdata = x[k == i,], type = "class")
            cm <- ifelse(x[k == i, y] == fcast,1,0)
        }
        if(model == 'nnet'){
            # Neural Net
            fit <- nnet(form, data = x[k != i,], size=10, maxit=250)    
            # print(fit)
            fcast <- predict(fit, newdata = x[k == i,], type = "class")
            cm <- ifelse(x[k == i, y] == fcast,1,0)
        }
        if(model == 'rf'){
            # Random Forest
            fit <- randomForest(form, data = x[k != i,], ntree=500)    
            # print(fit)
            fcast <- predict(fit, newdata = x[k == i,], type = "class")
            cm <- ifelse(x[k == i, y] == fcast,1,0)
        }
        if(model == 'svm'){
            # Random Forest
            fit <- svm(form, data = x[k != i,], ntree=500)    
            # print(fit)
            fcast <- predict(fit, newdata = x[k == i,], type = "class")
            cm <- ifelse(x[k == i, y] == fcast,1,0)
        }
        if(model == '2class'){
            train.data.full = data = x[k != i,]
            test.data.full = data = x[k == i,]

            # classifier 1

            train.data.full$same_as_past <- ifelse(train.data.full$actual_vote == train.data.full$mvar_1, 1, 0)
            train.data.full$same_as_past <- as.factor(train.data.full$same_as_past)

            fit <- nnet(form, train.data.full, size=10, maxit=250)
            fcast <- predict(fit, test.data.full, type="class")
            fcast <- as.factor(fcast)
            test.data.full$same_as_past <- fcast

            # classifier 2

            train.data.changed_vote <- train.data.full[train.data.full$same_as_past == 0,]
            test.data.changed_vote <- test.data.full[test.data.full$same_as_past == 0,]
            fit <- nnet(form2, train.data.changed_vote, size=10, maxit=250)
            fcast <- predict(fit, test.data.changed_vote, type="class")
            fcast <- as.factor(fcast)
            test.data.changed_vote$predicted_vote <- fcast
            test.data.same_vote <- test.data.full[test.data.full$same_as_past == 1,]
            test.data.same_vote$predicted_vote <- test.data.same_vote$mvar_1
            predicted.final = rbind(test.data.changed_vote,test.data.same_vote)
            
            cm <- ifelse(predicted.final$predicted_vote == predicted.final$actual_vote, 1,0)
        }
        
        accuracy <- sum(cm)/length(cm)
        vec.accuracy[i] <- accuracy

    }

    avg.accuracy <- mean(vec.accuracy)
    avg.error <- 1 - avg.accuracy
    cv <- data.frame(Accuracy = avg.accuracy, Error = avg.error)

    return(cv)
}
