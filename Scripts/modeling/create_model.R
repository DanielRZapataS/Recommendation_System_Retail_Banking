

#' Create model to recomed a bank product
#'
#' @param train_months : vector containing the first and last months which model
#'   will learn from costumer's behaivour, the will cut he master table
#'   (character)
#' @param test_month : single month which is use to test the model performance
#'   (character)
#' @param model_alias_modeling : Models name choosen by user (character)
#' @param model_type_modeling : product to be model (character)
#'
#' @return It directly writes the performance files and scores for each product
#'   in the model foldeer (model_alias_modeling)


create_model <- function(train_months,
                         test_month,
                         model_alias_modeling,
                         model_type_modeling) {
  print("Creating id model")
  data_id <- data.table(var = c("model_alias",
                                "model_type",
                                "train_months_since",
                                "train_months_to",
                                "dev_month",
                                "test_month"),
                        value = c(model_alias_modeling,
                                  model_type_modeling,
                                  train_months[1],
                                  train_months[2],
                                  dev_month,
                                  test_month)
                        ) 
  
  print("Upload master table")
  master <- get.path(master_path, "master") %>% readRDS()
  
  all_variables <- names(master)
  lags <- names(master)[grepl("month_ago", names(master))]
  lags_product <- lags[grepl(model_type_modeling, lags)]
  last_owned <- names(master)[grepl("last.owned", names(master))]
  last_owned_product <- last_owned[grepl(model_type_modeling, last_owned)]
  selected_var <- all_variables[all_variables %!in% c(lags, last_owned)]
  selected_var <- c(selected_var, lags_product, last_owned_product)
  master <- master[,  mget(selected_var)]

  
  print("Creating target variable")
  var_target <- paste0("pr_", model_type_modeling)
  target <-
    master[, .(llave,
               month.id = month.id - 2,
               var_target_2monthsFurther = get(var_target))]
  master <-
    merge(master,
          target,
          by = c("llave", "month.id"),
          all.x = TRUE)
  master[, target := ifelse(var_target_2monthsFurther - get(var_target) > 0, 1, 0)]
  master[, var_target_2monthsFurther := NULL]
  master[is.na(master)] <- 0
  rm(target)
  gc()
  
  # add in purchase frequency feature for each product
  print("Load purchase frequencies")
  
  purchase.frequencies <-
    readRDS(get.path(feature_path,  get_month(1)))
  
  purchase.frequencies <- data.table(purchase.frequencies)
  purchase_frequencies_products <-
    names(purchase.frequencies)[grepl(model_type_modeling, names(purchase.frequencies))]
  purchase.frequencies <-
    purchase.frequencies[, mget(c(
      "llave",
      "month.id",
      purchase_frequencies_products,
      "num.transactions"
    ))]
  
  
  master   <-
    merge(master,
          purchase.frequencies,
          by = c("month.id", "llave"),
          all.x = TRUE)
  rm(purchase.frequencies)
  gc()
  master[is.na(master)] <- 0
  
  
  # create train and test tables
  print("Creating train and test tables")
  # converting cutting months
  train_cut_max <- max(train_months)
  train_cut_max <-
    as.Date(paste0(as.character(train_cut_max), '01'), format = '%Y%m%d')
  train_cut_min <- min(train_months)
  train_cut_min <-
    as.Date(paste0(as.character(train_cut_min), '01'), format = '%Y%m%d')
  dev_cut <-
    as.Date(paste0(as.character(dev_month), '01'), format = '%Y%m%d')
  test_cut <-
    as.Date(paste0(as.character(test_month), '01'), format = '%Y%m%d')
  
  # divinding master table
  test <- master[periodo == test_cut]
  dev <- master[periodo == dev_cut]
  master <- master[periodo >= train_cut_min &
                     periodo <= train_cut_max]
  
  
  # Classifing variables into categories
  # there's a bunch of features related to the products, and thus they have similar
  # names. Separate them out to keep things straight
  
  id_variables <-
    c("llave", "periodo", "month.id", "month", "year", "target")
  products_variables <- names(master)[grepl("pr_", names(master))]
  products_variables <-
    c(products_variables, "total_products", "num.transactions")
  crm_vars <-
    names(master)[names(master) %!in% c(id_variables, products_variables)]
  
  categorical_cols <-
    c(crm_vars[sapply(master[, mget(crm_vars)], is.factor)],
      "month", "year")
  categorical_cols <-
    categorical_cols[categorical_cols %!in% c("bb_seg_comercial", "aa_cod_ocupacion")]
  
  numeric_cols <-
    c(crm_vars[!(sapply(master[, mget(crm_vars)], is.factor))],
      products_variables,
      c("bb_seg_comercial", "aa_cod_ocupacion"))
  
  print("One hot encoding")
  # one-hot encode the categorical features
  ohe <- dummyVars( ~ ., data = master[, mget(categorical_cols)])
  ohe <-
    as(data.matrix(predict(ohe, master[, mget(categorical_cols)])), "dgCMatrix")
  
  ohe_dev <- dummyVars( ~ ., data = dev[, mget(categorical_cols)])
  ohe_dev <- predict(ohe_dev, dev[, mget(categorical_cols)])
  ohe_dev <- as(data.matrix(ohe_dev), "dgCMatrix")
  
  ohe_test <- dummyVars( ~ ., data = test[, mget(categorical_cols)])
  ohe_test <- predict(ohe_test, test[, mget(categorical_cols)])
  ohe_cols <- colnames(ohe_test)
  ohe_test <- as(data.matrix(ohe_test), "dgCMatrix")
  
  print("Creating dgc matrixs")
  gc()
  # separate target
  target_train_dmatrix <-
    as(data.matrix(master$target), 'dgCMatrix')
  target_dev_dmatrix <-
    as(data.matrix(dev$target), 'dgCMatrix')
  
  # data to train and predict
  master_dmatrix         <-
    cbind(ohe, data.matrix(master[, mget(numeric_cols)]))
  rm(ohe)
  gc()
  dev_dmatrix         <-
    cbind(ohe_dev, data.matrix(dev[, mget(numeric_cols)]))
  rm(ohe_dev)
  gc()
  test_dmatrix       <-
    cbind(ohe_test, data.matrix(test[, mget(numeric_cols)]))
  rm(ohe_test)
  gc()
  
  dtrain <-
    xgb.DMatrix(data = master_dmatrix, label = target_train_dmatrix)
  ddev <- xgb.DMatrix(data = dev_dmatrix, label = target_dev_dmatrix)
  rm(master_dmatrix, target_train_dmatrix, dev_dmatrix, target_dev_dmatrix)
  gc()
  
  watchlist <- list(train = dtrain, test = ddev)
  # # set random seed for reproducibility
  set.seed(1104)
  
  cores <- detectCores() - 2
  # training model
  print("Training xgboost model")
  xgb.parameters <- list(booster = "gbtree",
                         objective = "binary:logistic",
                         max.depth = 6,
                         eta = 0.8,
                         eval_metric = "auc",
                         alpha = 1,
                         early_stoping_round = 30,
                         nrounds = 500)
  
  model <- xgb.train(
    data = dtrain,
    nround = xgb.parameters$nrounds,
    params = xgb.parameters,
    early_stopping_rounds = xgb.parameters$early_stoping_round,
    verbose = 1 ,
    nthread = cores, 
    watchlist = watchlist

  )
  
  # save model to binary local file
  print("Saving model")
  model_alias_path <-
    os.path.join(models_path, model_alias_modeling)
  dir.create(model_alias_path)
  fwrite(data_id, os.path.join(model_alias_path, paste0(model_alias_modeling, "_id.csv")))
  xgb.save(model, os.path.join(model_alias_path, paste0(model_alias_modeling, ".model")))
  # model <-  xgb.load(os.path.join(model_alias_path, paste0(model_alias_modeling, ".model")))
  
  # metrics model
  print("Making metrics model")
  
  cols <- c(ohe_cols, numeric_cols)
  importance_matrix <-
    xgb.importance(feature_names = cols, model = model)
  fwrite(importance_matrix,
         os.path.join(
           model_alias_path,
           paste0(model_alias_modeling, "_important_variables.csv")
         ))
  
  # Predict train and test
  master[, pred := predict(model, dtrain)]
  master <- calculate_pred(master, "target", "pred")
  fwrite(master[, .(llave, periodo, target, pred, pred_num)],
         os.path.join(model_alias_path, "pred_train.csv"))
  
  dev[, pred := predict(model, ddev)]
  dev <- calculate_pred(dev, "target", "pred")
  fwrite(dev[, .(llave, periodo, target, pred, pred_num)],
         os.path.join(model_alias_path, "pred_dev.csv"))
  
  test[, pred := predict(model, test_dmatrix)]
  test <- calculate_pred(test, "target", "pred")
  fwrite(test[, .(llave, periodo, target, pred, pred_num)],
         os.path.join(model_alias_path, "pred_test.csv"))
  

  
  metrics <-
    data.frame(train = c(
      accuracy(master$target, master$pred_num),
      Metrics::precision(master$target, master$pred_num),
      Metrics::recall(master$target, master$pred_num),
      Metrics::auc(master$target, master$pred_num)
    ),
    dev = c(
      accuracy(dev$target, dev$pred_num),
      Metrics::precision(dev$target, dev$pred_num),
      Metrics::recall(dev$target, dev$pred_num),
      Metrics::auc(master$target, master$pred_num)
    ),
    test = c(
      accuracy(test$target, test$pred_num),
      Metrics::precision(test$target, test$pred_num),
      Metrics::recall(test$target, test$pred_num),
      Metrics::auc(test$target, test$pred_num)
    )
    )
  rownames(metrics) <- c("accuracy", "precision", "recall", "auc")
  write.csv(metrics, os.path.join(model_alias_path, "metrics.csv"))
  
  performanceReport(master,
                    path = models_path,
                    modelFolder = model_alias_modeling,
                    alias = "train")
  performanceReport(dev,
                    path = models_path,
                    modelFolder = model_alias_modeling,
                    alias = "dev")
  performanceReport(test,
                    path = models_path,
                    modelFolder = model_alias_modeling,
                    alias = "test")
  
  no_quantil <-
    c("llave", "month.id", "aa_cod_ciiu", "departamento")
  quantil <- names(master)[names(master) %!in% no_quantil]
  exportQuantile(
    dt = test[, mget(quantil)],
    mostImp = importance_matrix ,
    outputPath = os.path.join(models_path, model_alias_modeling, "quantile_test.csv")
  )
  exportQuantile(
    dt = master[, mget(quantil)],
    mostImp = importance_matrix,
    outputPath = os.path.join(models_path, model_alias_modeling, "quantile_train.csv")
  )
  
}
