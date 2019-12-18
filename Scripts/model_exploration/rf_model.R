rf_model <- function(data_tesis_path,
                            products,
                            train_cut_max,
                            train_cut_min,
                            dev_cut,
                            rf_params,
                            i){
  ##### model meta data ####
  print("Making model's fold")
  gc()
  model_alias_modeling <-
    paste0("rf_model_", today() %>% format(., "%Y%m%d"), "_", products[i])
  model_type_modeling <- products[i]
  # model product folder
  model_alias_modeling <- os.path.join(models_path, model_alias_modeling)
  dir.create(model_alias_modeling)
  
  # models folder
  models_folder <- os.path.join(model_alias_modeling, "models_library")
  dir.create(models_folder)
  
  
  ##### load data ####
  print(paste("Loading data for", products[i]))
  dt_product_path <- os.path.join(data_tesis_path, products[i]) 
  data <- get.path(dt_product_path, products[i]) %>% readRDS()
  data <- data[periodo <= test_cut]
  data[, year := factor(year, levels = unique(year))]
  data[, aa_estrato := paste0("estr_", aa_estrato)]
  data[, aa_estrato := factor(aa_estrato, levels = paste0("estr_", 0:6))]
  data[, target := factor(target)]
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
  
  ##### getting best variables #####
  fold_model <-  list.files(models_path)
  fold_model <- grep("full_models", fold_model, value = T)
  fold_model <- grep(products[i], fold_model, value = T)
  best_variables <- list.files(os.path.join(models_path, fold_model))
  best_variables <- grep("important_variables.RData", best_variables, value = T)
  load(os.path.join(models_path, fold_model, best_variables))
  
 #### h2o data preparation ####
  # convert to h2o object
  class(master$target)
  master_h2o <-
    model.matrix(target ~ . - 1, data = master[, mget(final_cols)])
  dev_h2o <-
    model.matrix(target ~ . - 1, data = dev[, mget(final_cols)])
  test_h2o <-
    model.matrix(target ~ . - 1, data = test[, mget(final_cols)])
  master_dev_h2o <-
    model.matrix(target ~ . - 1, data = master_dev[, mget(final_cols)])
  
  master_h2o <- data.table(target = master$target, master_h2o)
  dev_h2o <- data.table(data.table(target = dev$target, dev_h2o))
  test_h2o <- data.table(data.table(target = test$target, test_h2o))
  master_dev_h2o <-
    data.table(data.table(target = master_dev$target, master_dev_h2o))
  
  names(master_h2o) <- tolower(names(master_h2o))
  names(master_h2o) <- 
    stri_trans_general(names(master_h2o),"Latin-ASCII")
  names(master_h2o) <- tm::removePunctuation(names(master_h2o))
  names(master_h2o) <- gsub(" ", "", names(master_h2o), fixed = TRUE)
  names(dev_h2o) <- names(master_h2o)
  names(test_h2o) <- names(master_h2o)
  names(master_dev_h2o) <- names(master_h2o)
  
  # selecting variables 
  # master_h2o <- as.h2o(master_h2o[, mget(c("target",imp_rf_vars))])
  # dev_h2o <- as.h2o(dev_h2o[, mget(c("target",imp_rf_vars))])
  # test_h2o <- as.h2o(test_h2o[, mget(c("target",imp_rf_vars))])
  master_h2o <- as.h2o(master_h2o)
  dev_h2o <- as.h2o(dev_h2o)
  test_h2o <- as.h2o(test_h2o)
  master_dev_h2o <- as.h2o(master_dev_h2o)
  
  y <- "target"
  x <- setdiff(names(master_h2o), y) 
  
  ##### training random forest model ####
  print("Trainig random forest models")
  
  # Train and validate a cartesian grid of RandomForests
  rf_grid <- h2o.grid(
    "randomForest",
    x = x,
    y = y,
    grid_id = "rf_grid",
    training_frame = master_h2o,
    validation_frame = dev_h2o,
    ntrees = 500,
    seed = 123,
    stopping_metric = "AUC",
    stopping_rounds = 10,
    stopping_tolerance = 0.005,
    hyper_params = rf_params
  )
  
  # Get the grid results, sorted by validation AUC
  rf_gridperf <- h2o.getGrid(grid_id = "rf_grid",
                               sort_by = "auc",
                               decreasing = TRUE)
  
  
  print(rf_gridperf)
  
  # Grab the top GBM model, chosen by validation AUC
  best_rf <- h2o.getModel(rf_gridperf@model_ids[[1]])
  
  # Now let's evaluate the model performance on a test set
  # so we get an honest estimate of top model performance
  # best_rf_perf <- h2o.performance(model = best_rf,
                                    # newdata = test_h2o)
  # h2o.auc(best_rf_perf)
  
  # Look at the hyperparamters for the best model
  best_parameters <- print(best_rf@model[["model_summary"]]) 
  rf_gridperf <- as.data.table(rf_gridperf@summary_table)
  saveRDS(best_parameters,
          file = os.path.join(model_alias_modeling, "best_parameters.rds"))
  saveRDS(rf_gridperf,
          file = os.path.join(model_alias_modeling, "rf_grid_pperformance.rds"))
  
  # r train rf 
  # random forest model
  best_rf <- h2o.randomForest(
    x = x, 
    y = y,
    training_frame = master_dev_h2o,
    ntrees = 500,
    max_depth =  as.numeric(rf_gridperf$max_depth[1]) ,
    sample_rate = as.numeric(rf_gridperf$sample_rate[1]),
    stopping_metric = "AUC",    
    seed = 123
  )

  h2o.saveModel(object = best_rf, path = models_folder, force = TRUE)
  h2o.saveModelDetails(object = best_rf,
                       path = models_folder, force = TRUE)
  
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
    imp_rf_vars,
    master_h2o,
    dev_h2o,
    test_h2o,
    master,
    dev,
    test,
    file = os.path.join(model_alias_modeling, paste0(products[i], "_rf_objects.RData"))
  )
  
  # metrics model
  print("Making metrics model")
  
  variable_importance_rf <- h2o.varimp(best_rf)
  variable_importance_rf <- data.table(variable_importance_rf)
  fwrite(variable_importance_rf,
         os.path.join(
           model_alias_modeling,"rf_important_variables.csv"
         ))
  #### Predict train and test ####
  rf_preds_master <- h2o.predict(best_rf, master_h2o) %>% as.data.frame()
  rf_preds_dev <- h2o.predict(best_rf, dev_h2o) %>% as.data.frame()
  rf_preds_test <- h2o.predict(best_rf, test_h2o) %>% as.data.frame()
  
  master[,pred := rf_preds_master$p1]
  dev[,pred := rf_preds_dev$p1]
  test[,pred := rf_preds_test$p1]
  
  master <- calculate_pred(master, "target", "pred")
  fwrite(master[, .(llave, periodo, target, pred, pred_num)],
         os.path.join(model_alias_modeling, "pred_train.csv"))
  
  dev <- calculate_pred(dev, "target", "pred")
  fwrite(dev[, .(llave, periodo, target, pred, pred_num)],
         os.path.join(model_alias_modeling, "pred_dev.csv"))
  
  test <- calculate_pred(test, "target", "pred")
  fwrite(test[, .(llave, periodo, target, pred, pred_num)],
         os.path.join(model_alias_modeling, "pred_test.csv"))
  #### calculating metrics ####
  master[, target := ifelse(target == 1, 1, 0)]
  dev[, target := ifelse(target == 1, 1, 0)]
  test[, target := ifelse(target == 1, 1, 0)]
  
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
      paste0("rf_model_", today() %>% format(., "%Y%m%d"), "_", products[i]),
    alias = "train"
  )
  performanceReport(
    dev,
    path = models_path,
    modelFolder =
      paste0("rf_model_", today() %>% format(., "%Y%m%d"), "_", products[i]),
    alias = "dev"
  )
  performanceReport(
    test,
    path = models_path,
    modelFolder = 
      paste0("rf_model_", today() %>% format(., "%Y%m%d"), "_", products[i]),
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
  # rm(rf_grid,
  #    rf_gridperf,
  #    rf_preds_dev,
  #    rf_preds_master,
  #    rf_preds_test,
  #    test, 
  #    test_h2o,
  #    uplift_table,
  #    variable_importance_rf,
  #    best_parameters,
  #    best_rf,
  #    master, 
  #    master_dev,
  #    master_h2o,
  #    master_dev_h2o,
  #    metrics,
  #    dev, 
  #    dev_h2o
  #    )
  gc()
}