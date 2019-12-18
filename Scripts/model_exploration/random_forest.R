mtry <- seq((ncol(master)-1)/3, ncol(master) * 0.8, 6) %>% round(0)
nodesize <- seq(3, 9, 3)

mtry <- c(45, 93)
nodesize <- 6

#'Train multiple Random Forest models chosen from random forest parameters to 
#'forecast one step ahead, and return Best Random forest model with the
#' lowest recursive forecast error on validation transactions dates.

#' @param master: Matrix with target and variables on training.
#' @param mtry: Vector of possible values for number of variables randomly 
#' sampled as candidates at each split.(numeric)
#' @param nodesize: Vector of possible values for minimum size of terminal 
#' nodes. (numeric)
#' @param sampsize: Vector of possible values of the size of sample to 
#' draw. (numeric)
#' @param target_val: Matrix on  validation. (numeric) 
#' @return List containing best random forest, auc, and recursive forecast

randomForest_train <- function(master, mtry, nodesize,
                               dev){
  require(pROC)
  require(randomForest)
  # Create a data frame containing all combinations 
  hyper_grid <- expand.grid(mtry = mtry, nodesize = nodesize)
  
  RFmodel<-list()
  # Write a loop over the rows of hyper_grid to train the grid of models
  for (i in 1:nrow(hyper_grid)) {
    
    # Train a Random Forest model
    RFmodel[[i]] <- randomForest(formula = target ~ ., 
                                 data = master,
                                 mtry = hyper_grid$mtry[i],
                                 nodesize = hyper_grid$nodesize[i],
                                 ntree = 10, 
                                 importance = T,
                                 keep.forst = T)
    
    
  }
  
  # Identify optimal set of hyperparmeters based on valdation error
  # Evaluate the grid 
  # Number of potential models in the grid
  num_models <- length(RFmodel)
  
  # Create an empty vector to store auc values
  auc_values_RF <- c()
  
  # save recursive forecast 
  predics_val <- list()
  
  for (i in 1:num_models) {
    
    # Retreive the i^th model from the list
    model <- RFmodel[[i]]
    
    # Generate predictions on grade_valid 
    pred <- predict(object = model,
                    newdata = dev)
    predics_val[[i]] <- pred
    
    roc_model <- ropROC::c(val$target, pred)
    

    # Compute validation auc and add to the 
    auc_values_RF[i] <- pROC::auc(xgb_auc_roc_obj_master)
    
    
  }
  
  names(auc_values_RF) <- c(paste0("RF", rep(1:nrow(hyper_grid))))
  
  # Identify the model with smallest validation set auc
  RF_model <- RFmodel[[which.max(auc_values_RF)]]
  auc_train <- auc(actual = master$target,
                     predicted = predict(RF_model) )
  auc_val <- auc_values_RF[which.max(auc_values_RF)]
  fit_train <- predict(RF_model)
  fit_val <-  predics_val[[which.max(auc_values_RF)]]
  
  return(list(auc_train = auc_train, auc_val = auc_val,
              fit_train = fit_train, fit_val = fit_val,
              model_name = paste0(RF_model$call)[1], 
              model = RF_model))
  
  
}
