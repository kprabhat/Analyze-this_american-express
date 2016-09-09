library('rpart')
library('nnet')
library('randomForest')
library('e1071')
library('caret')

source('src/preprocess.R')
source('src/crossval.R')
source('src/crossval2.R')
source('src/select.R')

" Read Data "

train.data.initial <- read.csv('data/Training_Dataset.csv', row.names=1)
test.data.initial <- read.csv('data/Leaderboard_Dataset.csv', row.names=1)
# final.data.initial <- read.csv('data/Final_Dataset.csv', row.names=1)

levels(test.data.initial$mvar_22)[levels(test.data.initial$mvar_22)=='professional'] <- 'professiol'
levels(test.data.initial$mvar_12)[levels(test.data.initial$mvar_12)=='Factory Manager'] <- 'Factory Mager'
levels(test.data.initial$mvar_12)[levels(test.data.initial$mvar_12)=='Middle Management'] <- 'Middle Magement'
levels(test.data.initial$mvar_12)[levels(test.data.initial$mvar_12)=='Senior Management'] <- 'Senior Magement'

summary(train.data.initial)
summary(test.data.initial)
# summary(final.data.initial)

" Preprocessing "

# Move actual vote column to the end of the data frame
actual_vote <- train.data.initial$actual_vote
train.data.initial$actual_vote <- NULL
train.data.initial <- cbind(train.data.initial, actual_vote)

train.data <- preprocess(train.data.initial)
test.data <- preprocess(test.data.initial, train = FALSE)

" Sample Data "

train.data.sample <- train.data[head(sample(nrow(train.data)), 100),]
test.data.sample <- test.data[head(sample(nrow(test.data)), 100),]

" Analysis "

# Confusion Matrix between actual and past votes
(confusion.matrix = confusionMatrix(train.data$actual_vote, train.data$mvar_1, dnn=c("Actual","Past")))

" Modelling "

formula.main <- actual_vote ~ .
formula.clue <- actual_vote ~ mvar_1 + mvar_23 + mvar_24 + mvar_25 + mvar_26 + mvar_28
formula.rel <- actual_vote ~ mvar_23_rel + mvar_24_rel + mvar_25_rel + mvar_26_rel + mvar_28_rel + mvar_1 + total_rallies
formula.selected <- actual_vote ~ mvar_1 + mvar_2 + mvar_3 + mvar_4 + mvar_5 + mvar_6 + mvar_7 + mvar_8 + mvar_9 + mvar_10 + mvar_11 + mvar_23 + mvar_25 + mvar_26 + mvar_28
formula.final <- formula.selected

train.data.full <- train.data
test.data.full <- test.data

# Try downsampling / upsampling to handle skewed classes
# train.data.full <- downSample(train.data.full, train.data.full$actual_vote)
# train.data.full <- upSample(train.data.full, train.data.full$actual_vote)

crossval(formula.clue, train.data.full, fold=5)

quit()
# Neural Net
fit <- nnet(formula.clue, train.data.full, size=10, maxit=250)
print(fit)
fcast <- predict(fit, test.data.full, type="class")
fcast <- as.data.frame(fcast)
row.names(fcast) <- row.names(test.data.full)
write.table(fcast, file='result/nnet_leaderboard_submission.csv', sep=',', col.names=FALSE)

# quit()

# 2-classifier-model

# classifier 1

train.data.full$same_as_past <- ifelse(train.data.full$actual_vote == train.data.full$mvar_1, 1, 0)
train.data.full$same_as_past <- as.factor(train.data.full$same_as_past)
formula.class1.selected <- same_as_past ~ mvar_1 + mvar_2 + mvar_3 + mvar_4 + mvar_5 + mvar_6 + mvar_7 + mvar_8 + mvar_9 + mvar_10 + mvar_23 + mvar_25 + mvar_26 + mvar_27 + mvar_28
formula.class1.clue <- same_as_past ~ mvar_1 + mvar_23 + mvar_24 + mvar_25 + mvar_26 + mvar_28
formula.class1.rel <- same_as_past ~ mvar_23_rel + mvar_24_rel + mvar_25_rel + mvar_26_rel + mvar_28_rel + mvar_1 + total_rallies
formula.selected.same_as_past <-  same_as_past ~ mvar_1 + mvar_28 + mvar_26 + mvar_23 + mvar_27 + mvar_2 + mvar_8 + mvar_3 + mvar_7 + mvar_25 + mvar_10 + mvar_6 + mvar_4 + mvar_9 + mvar_5

fit <- nnet(formula.class1.clue, train.data.full, size=10, maxit=250)
fcast <- predict(fit, test.data.full, type="class")
fcast <- as.factor(fcast)
test.data.full$same_as_past <- fcast

# classifier 2

train.data.changed_vote <- train.data.full[train.data.full$same_as_past == 0,]
test.data.changed_vote <- test.data.full[test.data.full$same_as_past == 0,]
fit <- nnet(formula.clue, train.data.changed_vote, size=10, maxit=250)
fcast <- predict(fit, test.data.changed_vote, type="class")
fcast <- as.factor(fcast)
# row.names(fcast) <- row.names(test.data.changed_vote)
test.data.changed_vote$predicted_vote <- fcast
test.data.same_vote <- test.data.full[test.data.full$same_as_past == 1,]
test.data.same_vote$predicted_vote <- test.data.same_vote$mvar_1
predicted.final = rbind(test.data.changed_vote,test.data.same_vote)
predicted.final = predicted.final['predicted_vote']
write.table(predicted.final, file='result/nnet_leaderboard_2_class.csv', sep=',', col.names=FALSE)

# 2-class crossval

result.cv <- crossval(form=formula.class1.clue,train.data.full,form2=formula.clue,fold=3,model='2class')
