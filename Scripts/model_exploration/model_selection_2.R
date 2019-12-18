
model_selection <- function(data_tesis_path,
                            products,
                            train_cut_max,
                            train_cut_min,
                            dev_cut,
                            i){
  ##### model meta data ####
  print("Making model's fold")
  gc()
  model_alias_modeling <-
    paste0("full_models_", today() %>% format(., "%Y%m%d"), "_", products[i])
  model_type_modeling <- products[i]
  model_alias_modeling <- os.path.join(models_path, "rf_models",model_alias_modeling)
  dir.create(model_alias_modeling)
  
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
  master <- data[periodo >= train_cut_min &
                   periodo <= train_cut_max]
  
  ##### Plots maker ####
  print(" making products plots")
  plots_path <- os.path.join("Plots", products[i])
  dir.create(plots_path)
  name_f <- "per_target"
  p <- master %>%
    group_by(target) %>%
    summarise(Count = n()) %>%
    mutate(percent = prop.table(Count) * 100) %>%
    ggplot(aes(reorder(target,-percent), percent), fill = target) +
    geom_col(fill = c("#FC4E07", "#E7B800")) +
    geom_text(
      aes(label = sprintf("%.2f%%", percent)),
      hjust = 0.01,
      vjust = -0.5,
      size = 3
    ) +
    theme_bw() +
    xlab(paste("Compradores de", products[i])) +
    ylab("Porcentaje") +
    ggtitle(paste("Porcentaje agregado de compradores de", products[i]))
  ggsave(plot = p,
         file = os.path.join(plots_path, paste0(products[i], "_", name_f, ".png")))
  
  name_f <- "per_target_mes"
  plot_base <- data %>% 
    group_by(periodo, target) %>% 
    summarise(Count = n())%>% 
    mutate(percent = prop.table(Count)*100) %>% data.table
  
  
  p <-
    ggplot(plot_base[target == 1], aes(periodo, percent), fill = periodo) +
    geom_col(fill = "#FC4E07") +
    geom_text(
      aes(label = sprintf("%.2f%%", percent)),
      hjust = 0.5,
      vjust = 1,
      size = 3,
      angle = -45
    ) +
    
    theme_bw() +
    xlab("Meses") +
    ylab("Porcentaje") +
    ggtitle(paste("Porcentaje de clientes compradores de", products[i]))
  ggsave(plot = p,
         file = os.path.join(plots_path, paste0(products[i], "_", name_f, ".png")))
  
  name_f <- "nivel_educacion"
  x_lab <- "Nivel educativo"
  p <- data %>%
    group_by(nivel_educativo, target) %>%
    summarise(Count = n()) %>%
    mutate(percent = prop.table(Count) * 100) %>% data.table
  p <- p[target == 1] %>% 
    ggplot(aes(x = nivel_educativo, y = percent, fill = target)) +
    geom_bar(stat = "identity") + geom_text(aes(
      x = nivel_educativo,
      y = percent,
      label = paste0(round(percent, 2), "%")
    ), size = 4) + theme_bw() +
    xlab(x_lab) +
    ylab("Porcentaje") +
    ggtitle(paste(
      "Porcentaje de clientes compradores por",
      tolower(x_lab),
      "en",
      products[i]
    ))
  
  ggsave(plot = p,
         file = os.path.join(plots_path, paste0(products[i], "_", name_f, ".png")))
  
  name_f <- "ocupacion"
  x_lab <- "Ocupacion"
  p <- data %>%
    group_by(aa_cod_ocupacion, target) %>%
    summarise(Count = n()) %>%
    mutate(percent = prop.table(Count) * 100) %>% data.table
  p <- p[target == 1] %>% 
    ggplot(aes(x = aa_cod_ocupacion, y = percent, fill = target)) +
    geom_bar(stat = "identity") + geom_text(aes(
      x = aa_cod_ocupacion,
      y = percent,
      label = paste0(round(percent, 2), "%")
    ), size = 4) + theme_bw() +
    xlab(x_lab) +
    ylab("Porcentaje") +
    ggtitle(paste(
      "Porcentaje de clientes compradores por",
      tolower(x_lab),
      "en",
      products[i]
    ))
  
  ggsave(plot = p,
         file = os.path.join(plots_path, paste0(products[i], "_", name_f, ".png")))
  
  t <- data %>%
    group_by(aa_cod_ocupacion, target) %>%
    summarise(Count = n()) %>%
    mutate(percent = prop.table(Count) * 100) %>% data.table
  t <- t[target == 1][order(-percent)]
  fwrite(t,  os.path.join(plots_path, paste0(products[i], "_", name_f, ".csv")))
  
  name_f <- "ciiu"
  x_lab <- "Codigo ciiu"
  p <- data %>%
    group_by(aa_cod_ciiu, target) %>%
    summarise(Count = n()) %>%
    mutate(percent = prop.table(Count) * 100) %>% data.table
  p <- p[target == 1] %>% 
    ggplot(aes(x = aa_cod_ciiu, y = percent, fill = target)) +
    geom_bar(stat = "identity") + geom_text(aes(
      x = aa_cod_ciiu,
      y = percent,
      label = paste0(round(percent, 2), "%")
    ), size = 4) + theme_bw() +
    xlab(x_lab) +
    ylab("Porcentaje") +
    ggtitle(paste(
      "Porcentaje de clientes compradores por",
      tolower(x_lab),
      "en",
      products[i]
    ))
  
  ggsave(plot = p,
         file = os.path.join(plots_path, paste0(products[i], "_", name_f, ".png")))
  t <- data %>%
    group_by(aa_cod_ciiu, target) %>%
    summarise(Count = n()) %>%
    mutate(percent = prop.table(Count) * 100) %>% data.table
  t <- t[target == 1][order(-percent)]
  fwrite(t,  os.path.join(plots_path, paste0(products[i], "_", name_f, ".csv")))
  
  name_f <- "departamento"
  x_lab <- "Departamento"
  p <- data %>%
    group_by(departamento, target) %>%
    summarise(Count = n()) %>%
    mutate(percent = prop.table(Count) * 100) %>% data.table 
  p <- p[target == 1] %>% 
    ggplot(aes(x = departamento, y = percent, fill = target)) +
    geom_bar(stat = "identity") + geom_text(aes(
      x = departamento,
      y = percent,
      label = paste0(round(percent, 2), "%")
    ), size = 4) + theme_bw() +
    xlab(x_lab) +
    ylab("Porcentaje") +
    ggtitle(paste(
      "Porcentaje de clientes compradores por",
      tolower(x_lab),
      "en",
      products[i]
    )) +
    theme(axis.text.x=element_text(angle=45, hjust=1))
  
  ggsave(plot = p,
         file = os.path.join(plots_path, paste0(products[i], "_", name_f, ".png")))
  t <- data %>%
    group_by(departamento, target) %>%
    summarise(Count = n()) %>%
    mutate(percent = prop.table(Count) * 100) %>% data.table
  t <- t[target == 1][order(-percent)]
  fwrite(t,  os.path.join(plots_path, paste0(products[i], "_", name_f, ".csv")))
  
  name_f <- "declara_renta"
  x_lab <- "Declara renta"
  p <- data %>%
    group_by(aa_declara_renta, target) %>%
    summarise(Count = n()) %>%
    mutate(percent = prop.table(Count) * 100) %>% data.table 
  p <- p[target == 1] %>% 
    ggplot(aes(x = aa_declara_renta, y = percent, fill = target)) +
    geom_bar(stat = "identity") + geom_text(aes(
      x = aa_declara_renta,
      y = percent,
      label = paste0(round(percent, 2), "%")
    ), size = 4) + theme_bw() +
    xlab(x_lab) +
    ylab("Porcentaje") +
    ggtitle(paste(
      "Porcentaje de clientes compradores por",
      tolower(x_lab),
      "en",
      products[i]
    ))
  
  ggsave(plot = p,
         file = os.path.join(plots_path, paste0(products[i], "_", name_f, ".png")))
  
  
  name_f <- "estrato"
  x_lab <- "Estrato socio económico"
  p <- data %>%
    group_by(aa_estrato, target) %>%
    summarise(Count = n()) %>%
    mutate(percent = prop.table(Count) * 100) %>% data.table
  p <- p[target == 1] %>% 
    ggplot(aes(x = aa_estrato, y = percent, fill = target)) +
    geom_bar(stat = "identity") + geom_text(aes(
      x = aa_estrato,
      y = percent,
      label = paste0(round(percent, 2), "%")
    ), size = 4) + theme_bw() +
    xlab(x_lab) +
    ylab("Porcentaje") +
    ggtitle(paste(
      "Porcentaje de clientes compradores por",
      tolower(x_lab),
      "en",
      products[i]
    ))
  
  ggsave(plot = p,
         file = os.path.join(plots_path, paste0(products[i], "_", name_f, ".png")))
  
  name_f <- "tipo_vivienda"
  p <- x_lab <- "Tipo vivienda"
  p <- data %>%
    group_by(aa_tipo_vivienda, target) %>%
    summarise(Count = n()) %>%
    mutate(percent = prop.table(Count) * 100) %>% data.table 
  p <- p[target == 1] %>% 
    ggplot(aes(x = aa_tipo_vivienda, y = percent, fill = target)) +
    geom_bar(stat = "identity") + geom_text(aes(
      x = aa_tipo_vivienda,
      y = percent,
      label = paste0(round(percent, 2), "%")
    ), size = 4) + theme_bw() +
    xlab(x_lab) +
    ylab("Porcentaje") +
    ggtitle(paste("Porcentaje de clientes compradores por", tolower(x_lab)))
  
  ggsave(plot = p,
         file = os.path.join(plots_path, paste0(products[i], "_", name_f, ".png")))
  
  name_f <- "estado_civil"
  p <- x_lab <- "Estado civil"
  p <- data %>%
    group_by(mar_status, target) %>%
    summarise(Count = n()) %>%
    mutate(percent = prop.table(Count) * 100) %>% data.table 
  p <- p[target == 1] %>% 
    ggplot(aes(x = mar_status, y = percent, fill = target)) +
    geom_bar(stat = "identity") + geom_text(aes(
      x = mar_status,
      y = percent,
      label = paste0(round(percent, 2), "%")
    ), size = 4) + theme_bw() +
    xlab(x_lab) +
    ylab("Porcentaje") +
    ggtitle(paste("Porcentaje de clientes compradores por", tolower(x_lab)))
  
  ggsave(plot = p,
         file = os.path.join(plots_path, paste0(products[i], "_", name_f, ".png")))
  
  name_f <- "sexo"
  x_lab <- "Sexo"
  p <- data %>%
    group_by(sex, target) %>%
    summarise(Count = n()) %>%
    mutate(percent = prop.table(Count) * 100) %>% data.table 
  p <- p[target == 1] %>% 
    ggplot(aes(x = sex, y = percent, fill = target)) +
    geom_bar(stat = "identity") + geom_text(aes(
      x = sex,
      y = percent,
      label = paste0(round(percent, 2), "%")
    ), size = 4) + theme_bw() +
    xlab(x_lab) +
    ylab("Porcentaje") +
    ggtitle(paste("Porcentaje de clientes compradores por", tolower(x_lab)))
  
  ggsave(plot = p,
         file = os.path.join(plots_path, paste0(products[i], "_", name_f, ".png")))
  
  name_f <- "nivel_segmento_comercial"
  x_lab <- "Segmento comercial"
  data %>%
    group_by(bb_seg_comercial, target) %>%
    summarise(Count = n()) %>%
    mutate(percent = prop.table(Count) * 100) %>% data.table 
  p <- p[target == 1] %>% 
    ggplot(aes(x = bb_seg_comercial, y = percent, fill = target)) +
    geom_bar(stat = "identity") + geom_text(aes(
      x = bb_seg_comercial,
      y = percent,
      label = paste0(round(percent, 2), "%")
    ), size = 4) + theme_bw() +
    xlab(x_lab) +
    ylab("Porcentaje") +
    ggtitle(paste("Porcentaje de clientes compradores por", tolower(x_lab)))
  
  ggsave(plot = p,
         file = os.path.join(plots_path, paste0(products[i], "_", name_f, ".png")))
  
  name_f <- "antiguedad"
  y_lab <- "Antigüedad"
  
  p <- ggplot(data, aes(y = antiguedad, x = "", fill = target)) +
    geom_boxplot() +
    theme_bw() +
    xlab(" ") +
    ylab(y_lab) +
    scale_y_continuous(labels = comma, limits = quantile(data[, get("antiguedad")], c(0.1, 0.9))) +
    ggtitle(paste("Porcentaje de clientes compradores por", tolower(y_lab)))
  
  ggsave(plot = p,
         file = os.path.join(plots_path, paste0(products[i], "_", name_f, ".png")))
  
  name_f <- "edad"
  y_lab <- "Edad"
  
  p <- ggplot(data, aes(y = age, x = "", fill = target)) +
    geom_boxplot() +
    theme_bw() +
    xlab(" ") +
    ylab(y_lab) +
    scale_y_continuous(labels = comma, limits = quantile(data[, get("age")], c(0.1, 0.9))) +
    ggtitle(paste("Porcentaje de clientes compradores por", tolower(y_lab)))
  
  ggsave(plot = p,
         file = os.path.join(plots_path, paste0(products[i], "_", name_f, ".png")))
  
  name_f <- "pasivos"
  y_lab <- "Valor de pasivos"
  
  p <-
    ggplot(data, aes(y = aa_vlr_pasivos, x = "", fill = target)) +
    geom_boxplot() +
    theme_bw() +
    xlab(" ") +
    ylab(y_lab) +
    scale_y_continuous(labels = comma, limits = quantile(data[, get("aa_vlr_pasivos")], c(0.1, 0.9))) +
    ggtitle(paste("Porcentaje de clientes compradores por", tolower(y_lab)))
  
  ggsave(plot = p,
         file = os.path.join(plots_path, paste0(products[i], "_", name_f, ".png")))
  
  name_f <- "egresos"
  y_lab <- "Valor de egresos mensuales"
  
  p <-
    ggplot(data, aes(y = aa_vlr_egreso_mes, x = "", fill = target)) +
    geom_boxplot() +
    theme_bw() +
    xlab(" ") +
    ylab(y_lab) +
    scale_y_continuous(labels = comma, limits = quantile(data[, get("aa_vlr_egreso_mes")], c(0.1, 0.9))) +
    ggtitle(paste("Porcentaje de clientes compradores por", tolower(y_lab)))
  
  ggsave(plot = p,
         file = os.path.join(plots_path, paste0(products[i], "_", name_f, ".png")))
  
  name_f <- "ingresos"
  y_lab <- "Valor de ingresos mensuales"
  
  p <-
    ggplot(data, aes(y = aa_vlr_ing_bru_mes, x = "", fill = target)) +
    geom_boxplot() +
    theme_bw() +
    xlab(" ") +
    ylab(y_lab) +
    scale_y_continuous(labels = comma, limits = quantile(data[, get("aa_vlr_ing_bru_mes")], c(0.1, 0.9))) +
    ggtitle(paste("Porcentaje de clientes compradores por", tolower(y_lab)))
  ggsave(plot = p,
         file = os.path.join(plots_path, paste0(products[i], "_", name_f, ".png")))
  
  name_f <- "activos"
  y_lab <- "Valor de activos"
  
  p <-
    ggplot(data, aes(y = aa_vlr_activos, x = "", fill = target)) +
    geom_boxplot() +
    theme_bw() +
    xlab(" ") +
    ylab(y_lab) +
    scale_y_continuous(labels = comma, limits = quantile(data[, get("aa_vlr_activos")], c(0.1, 0.9))) +
    ggtitle(paste("Porcentaje de clientes compradores por", tolower(y_lab)))
  ggsave(plot = p,
         file = os.path.join(plots_path, paste0(products[i], "_", name_f, ".png")))
  
  name_f <- "corr_financieras"
  data[, target_num := as.numeric(target)]
  data[, .N, by = target]
  fin_vars <- grep("vlr", names(data), value = T)
  fin_cor <- round(cor(data[, mget(c(fin_vars, "target_num"))]), 1)
  p <-
    ggcorrplot(fin_cor,  title = "Correlation") + theme(plot.title = element_text(hjust = 0.5))
  ggsave(plot = p,
         file = os.path.join(plots_path, paste0(products[i], "_", name_f, ".png")))
  
  name_f <- "corr_products"
  pr_vars <- grep("pr", names(data), value = T)
  pr_cor <- round(cor(data[, mget(c(pr_vars, "target_num"))]), 1)
  p <-  ggcorrplot(pr_cor,  title = "Correlation") + theme(plot.title = element_text(hjust = 0.5))
  ggsave(plot = p,
         file = os.path.join(plots_path, paste0(products[i], "_", name_f, ".png")))
  data[, target_num := NULL]
  rm(data)
  gc()
  
  
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
  
  ##### making xgb data ####  
  master_dmatrix <-
    sparse.model.matrix(target ~ . - 1, data = master[, mget(final_cols)])
  
  dev_dmatrix <-
    sparse.model.matrix(target ~ . - 1, data = dev[, mget(final_cols)])
  
  model_cols <- dev_dmatrix@Dimnames[[2]]
  
  
  # separate target
  target_train_dmatrix <-
    as(data.matrix(master$target), 'dgCMatrix')
  target_dev_dmatrix <-
    as(data.matrix(dev$target), 'dgCMatrix')
  
  dtrain <-
    xgb.DMatrix(data = master_dmatrix, label = target_train_dmatrix)
  ddev <- xgb.DMatrix(data = dev_dmatrix, label = target_dev_dmatrix)
  
  gc()
  rm(target_dev_dmatrix, target_train_dmatrix)
  watchlist <- list(train = dtrain, test = ddev)
  # # set random seed for reproducibility
  set.seed(1104)
  
  cores <- detectCores() - 1
  ##### training xgboost model ####
  # there are two ways of trainging xgboost models for a binary clasification
  # problems: using AUC of precision-recall curve or AUC of ROC. The firsth is
  # more usefull for unbalance classes as capture a the clients whose getting
  # financial product month through month.
  
  
  # xgboost maximizing Precision and recall AUC
  
  
  # xgboost maximizing AUC of ROC 
  
  print("Training xgboost model using ROC curve")
  
  xgb.parameters <- list(booster = "gbtree",
                         objective = "binary:logistic",
                         eval_metric = "auc",
                         early_stoping_round = 30,
                         nrounds = 500)
  
  model_xgb_auc <- xgb.train(
    data = dtrain,
    nround = xgb.parameters$nrounds,
    params = xgb.parameters,
    early_stopping_rounds = xgb.parameters$early_stoping_round,
    verbose = 1 ,
    nthread = cores, 
    watchlist = watchlist)
  
  xgb.save(model_xgb_auc, os.path.join(model_alias_modeling, "xgb_auc.model"))
  
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
    master,
    dev,
    file = os.path.join(model_alias_modeling, paste0(products[i], "_xgb_objects.RData"))
  )
  
  ##### data preparation for other models ####
  
  # convert to h2o object
  class(master$target)
  master_h2o <- as.h2o(master[, mget(final_cols)])
  dev_h2o <- as.h2o(dev[, mget(final_cols)])
  y <- "target"
  x <- setdiff(names(master_h2o), y) 
  
  # names_clean <- names(master)
  # names_clean <- stri_trans_general(names_clean,"Latin-ASCII")
  # names_clean <-  tm::removePunctuation(names_clean)
  # names_clean <- stri_replace_all_fixed(names_clean, " ", "")
  # names(master) <- names_clean
  # names(dev) <- names_clean
  # names(test) <- names_clean
  
  ##### Training h2o regression  ####
  
  
  print("Trainig h2o models")
  # random forest model
  rf <- h2o.randomForest(
    x = x, 
    y = y,
    training_frame = master_h2o,
    validation_frame = dev_h2o,
    ntrees = 500,
    stopping_metric = "AUC",    
    stopping_rounds = 10,         
    stopping_tolerance = 0.005,
    seed = 123
  )
  
  h2o.saveModel(object = rf, path = model_alias_modeling, force = TRUE)
  h2o.saveModelDetails(object = rf,
                       path = model_alias_modeling, force = TRUE)
    ##### Predicciones ####
  xgb_auc_preds_master <-  predict(model_xgb_auc, master_dmatrix)
  xgb_auc_preds_dev <-  predict(model_xgb_auc, dev_dmatrix)
  
  
  rf_preds_master <- h2o.predict(rf, master_h2o) %>% as.data.frame()
  rf_preds_dev <- h2o.predict(rf, dev_h2o) %>% as.data.frame()
  

  rf_performance_master <- h2o.performance(rf)
  rf_performance_dev <- h2o.performance(rf, dev_h2o)
  

  save(
    rf_performance_master,
    rf_performance_dev,
    file = os.path.join(model_alias_modeling, paste0(products[i], "_performance.RData"))
  )
  rm(
    rf_performance_master,
    rf_performance_dev,
  )
  gc()
  # exportar probabilidades 
  master[,':=' (xgb_auc_pred = xgb_auc_preds_master,
                rf_pred = rf_preds_master$p0,
  )
  ]
  dev[,':=' (xgb_auc_pred = xgb_auc_preds_dev,
             rf_pred = rf_preds_dev$p0,
  )
  ]
  
  fwrite(master[, .(llave, periodo, target, xgb_auc_pred, rf_pred)],
         os.path.join(model_alias_modeling, "pred_train.csv"))
  fwrite(dev[, .(llave, periodo, target, xgb_auc_pred,rf_pred)],
         os.path.join(model_alias_modeling, "pred_dev.csv"))
  
  rm(
    rf_preds_master,
    xgb_auc_preds_master,
    rf_preds_dev,
    xgb_auc_preds_dev
  )
  gc()
  # Important variables 
  importance_matrix <-
    xgb.importance(feature_names =  model_cols, model = model_xgb_auc)
  fwrite(importance_matrix,
         os.path.join(
           model_alias_modeling,"xgb_auc_important_variables.csv"
         ))
  
  variable_importance_rf <- h2o.varimp(rf)
  variable_importance_rf <- data.table(variable_importance_rf)
  fwrite(variable_importance_rf,
         os.path.join(
           model_alias_modeling,"rf_important_variables.csv"
         ))
  
  
  # build explainers on dev data
  
  
  # create custom predict function
  pred <- function(model, newdata)  {
    results <- as.data.frame(h2o.predict(model, as.h2o(newdata)))
    return(results[[2L]])
  }

  # random forest explainer
  explainer_rf <- explain(
    model = rf,
    data = as.data.frame(dev_h2o)[, -1],
    y = as.vector(as.numeric(dev$target)),
    predict_function = pred,
    label = "h2o rf"
  )
  
  
  explainer_xgb_auc <- explain(
    model_xgb_auc,
    data = dev_dmatrix,
    y = dev$target == 1,
    label = "xgboost auc"
  )
  
  save(
    explainer_rf,
    explainer_xgb_auc,
    file = os.path.join(model_alias_modeling, paste0(products[i], "_explainers.RData"))
  )
  
  rm(
    master,
    master_dmatrix,
    master_h2o
  )
  gc()
  ##### residual analysis ####
  resids_rf  <- model_performance(explainer_rf)
  resids_xgb_auc <- model_performance(explainer_xgb_auc)
  
  save(
    resids_glm,
    resids_rf,
    resids_gbm,
    resids_xgb_auc,
    resids_xgb_pr,
    file = os.path.join(model_alias_modeling, paste0(products[i], "_resids.RData"))
  )
  rm(
    resids_glm,
    resids_rf,
    resids_gbm,
    resids_xgb_auc,
    resids_xgb_pr
  )
  gc()
  ##### variable importace by permutation ####
 vip_rf  <-
    variable_importance(explainer_rf, n_sample = -1, loss_function = loss_root_mean_square)

    vip_xgb_auc <-
    variable_importance(explainer_xgb_auc,
                        n_sample = -1,
                        loss_function = loss_root_mean_square)
  
  save(
    vip_rf,
    vip_xgb_auc,
    file = os.path.join(model_alias_modeling, paste0(products[i], "_vip.RData"))
  )
  
  # plot explainers 
  
  p1 <- plot(vip_rf,max_vars = 20, show_baseline = T)
  ggsave(filename = os.path.join(model_alias_modeling, paste0(products[i], "_vip_rf.png")), plot = p1)
  
  p1 <- plot(vip_rf,max_vars = 20)
  ggsave(filename = os.path.join(model_alias_modeling, paste0(products[i], "_vip_rf1.png")), plot = p1)
  
  p1 <- plot(vip_xgb_auc,max_vars = 20, show_baseline = T)
  ggsave(filename = os.path.join(model_alias_modeling, paste0(products[i], "_vip_xgboost_auc.png")), plot = p1)
  
  p1 <- plot(vip_xgb_auc,max_vars = 20)
  ggsave(filename = os.path.join(model_alias_modeling, paste0(products[i], "_vip_xgboost_auc1.png")), plot = p1)
  
  
  # Gain
  p1 <- xgb.ggplot.importance(importance_matrix, measure = "Gain", rel_to_first = FALSE, top_n = 20)
  p1 <- p1 + ggplot2::ylab("Gain")
  ggsave(filename = os.path.join(model_alias_modeling, paste0(products[i], "_gain_xgboost_auc.png")), plot = p1)
  
  # Cover
  p1 <- xgb.ggplot.importance(importance_matrix, measure = "Cover", rel_to_first = FALSE, top_n = 20)
  p1 <- p1 + ggplot2::ylab("Cover")
  ggsave(filename = os.path.join(model_alias_modeling, paste0(products[i], "_cover_xgboost_auc.png")), plot = p1)
  
  # Frequency
  p1 <- xgb.ggplot.importance(importance_matrix, measure = "Frequency", rel_to_first = FALSE, top_n = 20)
  p1 <- p1 + ggplot2::ylab("Frequency")
  ggsave(filename = os.path.join(model_alias_modeling, paste0(products[i], "_frequency_xgboost_auc.png")), plot = p1)
  
  rm(p1)
  
  ppi = 300
  png(
    os.path.join(model_alias_modeling, paste0(products[i], "_rf_variable_importance.png")),
    width = 7 * ppi,
    height = 7 * ppi,
    res = ppi
  )
  h2o.varimp_plot(rf, num_of_features = 20)
  dev.off()
  
  ##### selectin variables ####
  
  # selecting variables by permutations
  vip_xgb_auc_table <- data.table(vip_xgb_auc)
  vip_xgb_auc_table <- vip_xgb_auc_table[order(- dropout_loss)]
  var_vip <- vip_xgb_auc_table[1:20, variable]
  var_vip <- var_vip[var_vip %!in% "_baseline_"] %>% as.character()

  vip_rf_table <- data.table(vip_rf)
  vip_rf_table <- vip_rf_table[order(- dropout_loss)]
  var_vip_rf <- vip_rf_table[1:20, variable]
  var_vip_rf <- var_vip[var_vip %!in% "_baseline_"] %>% as.character()

    
  # selectin variables by gain 
  var_gain <- importance_matrix[order(-Gain)]
  var_gain <- var_gain[1:20, Feature] %>% as.character()
  
  # selectin variables by cover 
  var_cover <- importance_matrix[order(-Cover)]
  var_cover <- var_cover[1:20, Feature] %>% as.character()
  
  # selectin variables by frequency
  var_freq <- importance_matrix[order(-Frequency)]
  var_freq <- var_freq[1:20, Feature] %>% as.character()
  
  # selectin variables of RF
  var_rf <- variable_importance_rf[order(-relative_importance)]
  var_rf <- var_rf[1:20, variable] %>% as.character()
  
  final_variables <- c(var_cover, var_freq, var_gain, var_rf, var_vip, var_vip_rf)
  
  final_variables <- unique( final_variables)
  
  saveRDS(object = final_variables, file = os.path.join(model_alias_modeling, paste0(products[i], "_final_variables.rds")))
  
  common_vars <- var_vip[var_vip %in% var_rf]
  for (j in common_vars) {
    pdp_xgb <-
      variable_response(explainer_xgb_auc, variable =  j, type = "pdp")
    pdp_rf <-
      variable_response(explainer_rf, variable =  j, type = "pdp")
    p1 <- plot(pdp_xgb, pdp_rf)
    ggsave(plot = p1,
           file = os.path.join(model_alias_modeling, paste0(products[i], "_", j, "_pdp.png")))}
  
  
  
  rm(
    explainer_rf,
    explainer_xgb_auc
  )
  rm(
    vip_rf,
    vip_xgb_auc
  )
  rm(
    model_xgb_auc,
    model_xgb_pr,
    glm,
    rf
  )
  gc()
  rm(dev,
     dev_dmatrix,
     dev_h2o)
  gc()
}