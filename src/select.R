feature.select <- function(form, train.data) {
    train.data.full <- train.data
    train.data.full[is.na(train.data.full)] <- 0
    result.rf <- randomForest(form, train.data.full, keep.forest=FALSE, importance=TRUE)
    feature.weights <- as.data.frame(result.rf$importance)
    feature.weights.sorted <- feature.weights[order(-feature.weights$MeanDecreaseAccuracy),]
    features.selected <- head(rownames(feature.weights.sorted), 15)
    fstring = paste(features.selected, collapse=' + ')
    fstring = paste('actual_vote', fstring, sep=' ~ ')
    formula.selected = as.formula(fstring)
    return(formula.selected)
}
