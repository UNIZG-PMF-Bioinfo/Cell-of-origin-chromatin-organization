# Library
library(data.table)
library(magrittr)
library(caret)

# Prediction of cell-of-origin
rf_mode_coo <- function(select_predictor, input_chrom, input_mut) {
  # Make predictors
  dt_temp <- input_chrom[celltype == select_predictor] %>%
    dcast(., interval ~ modification, value.var = "value", fill = 0) %>%
    merge(., input_mut, by="interval", all.x=TRUE)
  # make predictor
  X  <- dt_temp[,-c("interval", "N")]
  # Make response
  Y <- dt_temp[, N]
  # Define cross-validation
  set.seed(123)
  k_folds <- createFolds(Y, k = 10, returnTrain = T)
  my_cv <- trainControl(method = "cv", number = 10, index = k_folds )
  # Run random forest model
  rf <- train(x = X ,
              y = Y ,
              importance = "none",
              seed = 41,
              method = "ranger",
              num.trees = 500,
              trControl = my_cv )
  # Get output results
  rf_output <- 
    list(result = rf$results, 
         prediction = rf$finalModel$predictions, 
         outcome = rf$trainingData$.outcome, 
         finalrsq = rf$finalModel$r.squared, 
         cv.rsquared = rf$resample$Rsquared,
         index = rf$control$indexOut)
  ### return
  return(rf_output)
}
