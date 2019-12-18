

xgb_tunning_models <- function(data_tesis_path,
                            products,
                            train_cut_max,
                            train_cut_min,
                            dev_cut,
                            searchGridSubCol,
                            i){
  ##### model meta data ####
  print("Making model's fold")
  gc()
  model_alias_modeling <-
    paste0("xgb_model_", today() %>% format(., "%Y%m%d"), "_", products[i])
  model_type_modeling <- products[i]
  # model product folder
  model_alias_modeling <- os.path.join(models_path, model_alias_modeling)
  dir.create(model_alias_modeling)
  
  # models folder
  models_folder <- os.path.join(model_alias_modeling, "models_library")
  dir.create(models_folder)
  
  # plots folder 
  plots_folder <- os.path.join(model_alias_modeling, "plots")
  dir.create(plots_folder)
  
  ##### load data ####
  print(paste("Loading data for", products[i]))
  dt_product_path <- os.path.join(data_tesis_path, products[i]) 
  data <- get.path(dt_product_path, products[i]) %>% readRDS()
  data <- data[periodo <= test_cut]
  data[, year := factor(year, levels = unique(year))]
  data[, aa_estrato := paste0("estr_", aa_estrato)]
  data[, aa_estrato := factor(aa_estrato, levels = paste0("estr_", 0:6))]
  # data[, target := factor(target)]
  # divinding master table
  dev <- data[periodo == dev_cut]
  test <- data[periodo == test_cut]
  master <- data[periodo >= train_cut_min &
                   periodo <= train_cut_max]
  master_dev <- data[periodo >= train_cut_min &
                                 periodo <= dev_cut]
  rm(data)
  

  ##### separating variabbles ####
  
  id_variables <-
    c("llave", "periodo", "month.id", "month", "year", "target")
  products_variables <- names(master)[grepl("pr_", names(master))]
  products_variables <-
    c(products_variables, "total_products", "num.transactions")
  crm_vars <-
    names(master)[names(master) %!in% c(id_variables, products_variables)]
  
  categorical_cols <-
    c(crm_vars[sapply(master[, mget(crm_vars)], is.factor)])
  
  numeric_cols <-
    c(crm_vars[!(sapply(master[, mget(crm_vars)], is.factor))],
      products_variables)
  
  print("One hot encoding")
  # one-hot encode the categorical features
  final_cols <- c("target",categorical_cols, numeric_cols)
  
  # getting best variables 
  fold_model <-  list.files(models_path)
  fold_model <- grep("full_models", fold_model, value = T)
  fold_model <- grep(products[i], fold_model, value = T)
  best_variables <- list.files(os.path.join(models_path, fold_model))
  best_variables <- grep("important_variables.RData", best_variables, value = T)
  load(os.path.join(models_path, fold_model, best_variables))
    
  ##### making xgb data ####  
  master_dmatrix <- master[, mget(final_cols)]
  master_dmatrix <-
    sparse.model.matrix(target ~ . - 1, data = master_dmatrix)
  
  dev_dmatrix <- dev[, mget(final_cols)]
  dev_dmatrix <- 
    sparse.model.matrix(target ~ . - 1, data = dev_dmatrix)
  
  test_dmatrix <- test[, mget(final_cols)]
  test_dmatrix <- 
    sparse.model.matrix(target ~ . - 1, data = test_dmatrix)
  
  master_dev_dmatrix <- master_dev[, mget(final_cols)]
  master_dev_dmatrix <- 
    sparse.model.matrix(target ~ . - 1, data = master_dev_dmatrix)
  
  # manipulating dmatrix names 
  model_cols <- master_dmatrix@Dimnames[[2]]
  model_cols <- tolower(model_cols)
  model_cols <- 
    stri_trans_general(model_cols,"Latin-ASCII")
  model_cols <- tm::removePunctuation(model_cols)
  model_cols <- gsub(" ", "", model_cols, fixed = TRUE)
  
  master_dmatrix@Dimnames[[2]] <- model_cols
  dev_dmatrix@Dimnames[[2]] <- model_cols
  test_dmatrix@Dimnames[[2]] <- model_cols
  master_dev_dmatrix@Dimnames[[2]] <- model_cols
  
  # croosing with best variables analisis 
  best_variables <- paste(imp_xgb_vars,  collapse = "|")
  master_dmatrix <- master_dmatrix[, c(grep(best_variables , model_cols))]
  dev_dmatrix <- dev_dmatrix[, c(grep(best_variables , model_cols))]
  test_dmatrix <- test_dmatrix[, c(grep(best_variables , model_cols))]
  master_dev_dmatrix <- master_dev_dmatrix[, c(grep(best_variables , model_cols))]


  # separate target
  target_train_dmatrix <-
    as(data.matrix(master$target), 'dgCMatrix')
  target_dev_dmatrix <-
    as(data.matrix(dev$target), 'dgCMatrix')
  target_train_dev_dmatrix <-
    as(data.matrix(master_dev$target), 'dgCMatrix')
  
  dtrain <-
    xgb.DMatrix(data = master_dmatrix, label = target_train_dmatrix)
  ddev <- xgb.DMatrix(data = dev_dmatrix, label = target_dev_dmatrix)
  dtrain_dev <- xgb.DMatrix(data = master_dev_dmatrix, 
                            label = target_train_dev_dmatrix)
  
  gc()
  rm(target_dev_dmatrix, target_train_dmatrix)
  watchlist <- list(train = dtrain, test = ddev)
  # # set random seed for reproducibility
  set.seed(1104)
  
  cores <- detectCores() - 1
  ##### tinning xgboost arameters ####
  
  #Tuning de parametros
  
  print("Tuning parameters")
  
  system.time(ErrorsHyperparameters <-
                apply(searchGridSubCol, 1, function(parameterList) {
                  current_max_depth <- parameterList[["max_depth"]]
                  current_eta <- parameterList[["eta"]]
                  current_gamma <- parameterList[["gamma"]]
                  current_early_stoping_round <-
                    parameterList[["early_stoping_round"]]
                  current_nrounds <- parameterList[["nrounds"]]
                  
                  xgb.parameters_tuning <- list(
                    booster = "gbtree",
                    objective = "binary:logistic",
                    max.depth = current_max_depth,
                    eta = current_eta,
                    gamma = current_gamma,
                    eval_metric = "auc",
                    early_stoping_round = current_early_stoping_round,
                    nrounds = current_nrounds
                  )
                  
                  model_tuning <- xgb.train(
                    data = dtrain,
                    nround = xgb.parameters_tuning$nrounds,
                    params = xgb.parameters_tuning,
                    early_stopping_rounds = 
                      xgb.parameters_tuning$early_stoping_round,
                    verbose = 1,
                    watchlist = watchlist,
                    nthread = cores
                  )
                  
                  xvalidationScores <-
                    as.data.frame(model_tuning$evaluation_log)
                  xvalidationScores <- data.table(xvalidationScores)
                  xvalidationScores <-
                    xvalidationScores[order(-test_auc)]
                  
                  valaucpr <- xvalidationScores$test_auc[1]
                  trainaucpr <- xvalidationScores$train_auc[1]
                  best_iteration <- model_tuning$best_iteration
                  
                  output <-
                    return(
                      c(
                        valaucpr,
                        trainaucpr,
                        current_max_depth,
                        current_eta,
                        current_gamma,
                        current_early_stoping_round,
                        current_nrounds,
                        best_iteration
                      )
                    )
                }))
  output <- as.data.frame(t(ErrorsHyperparameters))
  varnames <-
    c(
      "val_auc",
      "tarin_auc",
      "max_depth",
      "eta",
      "gamma",
      "early_stoping",
      "nrounds",
      "best_iteration"
    )
  names(output) <- varnames
  
  output <- data.table(output)
  output <- output[order(-val_auc, -val_auc)]
  
  print("Best 5 results:")
  print(head(output, 5))
  
  saveRDS(output, os.path.join(model_alias_modeling, "best_tuning.rds"))

  # training model
  print("Training xgboost model")
  
  xgb.parameters <- list(booster = "gbtree",
                         objective = "binary:logistic",
                         max.depth = output$max_depth[1],
                         eta = output$eta[1],
                         gamma = output$gamma[1],
                         eval_metric = "auc", 
                         nrounds = output$best_iteration[1]) 
  
  model_xgb <- xgb.train(
    data = dtrain_dev,
    nround = xgb.parameters$nrounds,
    params = xgb.parameters,
    verbose = 1 ,
    nthread = cores
    
  )
  xgb.save(model_xgb, os.path.join(models_folder, "xgb.model"))
  output <- output[1]
  # output[, best_iterartion := model_xgb$best_iteration]
  output[, nfeatures := model_xgb$nfeatures]
  fwrite(output, os.path.join(model_alias_modeling, "model_parameters.csv"))
  
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
  
  fwrite(data_id, os.path.join(model_alias_modeling, "model_id.csv"))
  
  
  save(
    final_cols, 
    model_cols,
    master_dmatrix,
    dev_dmatrix,
    test_dmatrix,
    master,
    dev,
    test,
    file = os.path.join(model_alias_modeling, paste0(products[i], "_xgb_objects.RData"))
  )
  
  # metrics model
  print("Making metrics model")
  
  importance_matrix <-
    xgb.importance( model = model_xgb)
  fwrite(importance_matrix,
         os.path.join(
           model_alias_modeling,
           paste0(products[i], "_important_variables.csv")
         ))
  
  # Predict train and test
  
  master[, pred := predict(model_xgb, dtrain)]
  master <- calculate_pred(master, "target", "pred")
  fwrite(master[, .(llave, periodo, target, pred, pred_num)],
         os.path.join(model_alias_modeling, "pred_train.csv"))
  
  dev[, pred := predict(model_xgb, ddev)]
  dev <- calculate_pred(dev, "target", "pred")
  fwrite(dev[, .(llave, periodo, target, pred, pred_num)],
         os.path.join(model_alias_modeling, "pred_dev.csv"))
  
  test[, pred := predict(model_xgb, test_dmatrix)]
  test <- calculate_pred(test, "target", "pred")
  fwrite(test[, .(llave, periodo, target, pred, pred_num)],
         os.path.join(model_alias_modeling, "pred_test.csv"))
  
  metrics <-
    data.frame(train = c(
      accuracy(master$target, master$pred_num),
      Metrics::precision(master$target, master$pred_num),
      Metrics::recall(master$target, master$pred_num),
      Metrics::auc(master$target, master$pred)
    ),
    dev = c(
      accuracy(dev$target, dev$pred_num),
      Metrics::precision(dev$target, dev$pred_num),
      Metrics::recall(dev$target, dev$pred_num),
      Metrics::auc(dev$target, dev$pred)
    ),
    test = c(
      accuracy(test$target, test$pred_num),
      Metrics::precision(test$target, test$pred_num),
      Metrics::recall(test$target, test$pred_num),
      Metrics::auc(test$target, test$pred)
    )
    )
  rownames(metrics) <- c("accuracy", "precision", "recall", "auc")
  write.csv(metrics, os.path.join(model_alias_modeling, "metrics.csv"))
  
  performanceReport(
    master,
    path = models_path,
    modelFolder = 
      paste0("xgb_model_", today() %>% format(., "%Y%m%d"), "_", products[i]),
    alias = "train"
  )
  performanceReport(
    dev,
    path = models_path,
    modelFolder =
      paste0("xgb_model_", today() %>% format(., "%Y%m%d"), "_", products[i]),
    alias = "dev"
  )
  performanceReport(
    test,
    path = models_path,
    modelFolder = 
      paste0("xgb_model_", today() %>% format(., "%Y%m%d"), "_", products[i]),
    alias = "test"
  )
  uplift_table <- fread(get.path(model_alias_modeling, "test_uplift"))
  metrics_test <- metrics[, "test"]
  metrics_test <-
    c(metrics_test, uplift_table[porcentajesAnalizados %in% c(10, 20), Uplift.Acumulado])
  names(metrics_test) <-
    c("accuracy",
      "precision",
      "recall",
      "auc",
      "uplift_10",
      "uplift_20")
  
  write.csv(metrics_test, os.path.join(model_alias_modeling, "metrics_test.csv"))
  rm(master, 
     test, 
     dev,
     master_dev,
     master_dmatrix,
     master_dev_dmatrix,
     dev_dmatrix,
     ddev,
     dtrain_dev,
     dtrain,
     test_dmatrix,
     model_xgb,
     ErrorsHyperparameters,
     target_train_dev_dmatrix, 
     uplift_table,
     importance_matrix,
     metrics,
     output,
     watchlist
     
  )
gc()
}