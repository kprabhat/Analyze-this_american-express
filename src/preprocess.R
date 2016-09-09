getNum <- function(s) as.numeric(gsub('[^[:digit:]]', '', s))

calc_norm <- function(x) (x - min(x)) / (max(x) - min(x))

normalize <- function(data){
    data[is.na(data)] <- 0
    features.numeric <- sapply(data, is.numeric)
    data.numeric <- sapply(data[, features.numeric], calc_norm)
    data <- cbind(data[!features.numeric], data.numeric)
    return(data)
}

preprocess <- function(data, train=TRUE) {
    # Convert donations to integer type
    data[, 2:6] <- sapply(data[, 2:6], getNum)

    # Convert education level to two factors
    data$mvar_22 <- as.factor(data$mvar_22 == 'primary')

    data$mvar_16 <- as.factor(data$mvar_16) # Married Indicator
    data$mvar_17 <- as.factor(data$mvar_17) # Home Ownership Indicator

    # features.missing <- c('mvar_13', 'mvar_30') # Region Code and Primary Income
    # rdata <- data[!(colnames(data) %in% features.missing)]
    # rdata[is.na(rdata)] <- 0
    # data[!(colnames(data) %in% features.missing)] <- rdata

    data[is.na(data)] <- 0

    # data$total_rallies <- data$mvar_23 + data$mvar_24 + data$mvar_25 + data$mvar_26 + data$mvar_28
    # data$mvar_23_rel <- ifelse(data$total_rallies, data$mvar_23 / data$total_rallies, 0)
    # data$mvar_24_rel <- ifelse(data$total_rallies, data$mvar_24 / data$total_rallies, 0)
    # data$mvar_25_rel <- ifelse(data$total_rallies, data$mvar_25 / data$total_rallies, 0)
    # data$mvar_26_rel <- ifelse(data$total_rallies, data$mvar_26 / data$total_rallies, 0)
    # data$mvar_28_rel <- ifelse(data$total_rallies, data$mvar_28 / data$total_rallies, 0)

    # data$total_donations <- data$mvar_2 + data$mvar_3 + data$mvar_4 + data$mvar_5 + data$mvar_6
    # data$mvar_2_rel <- ifelse(data$total_donations, data$mvar_3 / data$total_donations, 0)
    # data$mvar_3_rel <- ifelse(data$total_donations, data$mvar_3 / data$total_donations, 0)
    # data$mvar_5_rel <- ifelse(data$total_donations, data$mvar_5 / data$total_donations, 0)
    # data$mvar_6_rel <- ifelse(data$total_donations, data$mvar_6 / data$total_donations, 0)
    # data$mvar_4_rel <- ifelse(data$total_donations, data$mvar_4 / data$total_donations, 0)

    # data$total_social_shares <- data$mvar_7 + data$mvar_8 + data$mvar_9 + data$mvar_10 + data$mvar_11
    # data$mvar_7_rel <- ifelse(data$total_social_shares, data$mvar_7 / data$total_social_shares, 0)
    # data$mvar_8_rel <- ifelse(data$total_social_shares, data$mvar_8 / data$total_social_shares, 0)
    # data$mvar_9_rel <- ifelse(data$total_social_shares, data$mvar_9 / data$total_social_shares, 0)
    # data$mvar_10_rel <- ifelse(data$total_social_shares, data$mvar_10 / data$total_social_shares, 0)
    # data$mvar_11_rel <- ifelse(data$total_social_shares, data$mvar_11 / data$total_social_shares, 0)

    if(train == TRUE){
    
      # Remove the rows which have social share as NA
        train.data.initial <- train.data.initial[!is.na(train.data.initial$mvar_10), ]
    
    #     data$total_rallies <- data$mvar_23 + data$mvar_24 + data$mvar_25 + data$mvar_26 + data$mvar_28
    #     deleted_rows <- nrow(data) - sum(data$total_rallies == data$mvar_27)
    #     deleted_rows <- (deleted_rows * 100) / nrow(data)
    #     print(paste(deleted_rows, '% rows deleted due to discrepancy in total number of rallies.'))
    #     data <- data[data$total_rallies == data$mvar_27,]
    #     data$mvar_27 <- NULL
    
    }

    return(data)
}
