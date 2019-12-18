## Starting 

# Clean environment
rm(list=ls())
gc()
# Disable scientific notation
options(scipen=999)
# Change prompt
options(prompt="NPB_V0.3> ", continue=" ") 

# Load utilities functions (change wd, auxiliary scripts...)
source("Scripts/utiles.R")
set_environment()
library(Matrix)

data_tesis_path <- os.path.join(data_path, "Temporary")

products <<- c("tcredito", "crediservice", "ahorros", "cdt", "vehiculo", 
               "libranza", "libredestino", "nomina", "vivienda")

train_months <<- c(get_month(22), get_month(5))
dev_month <<- get_month(4)
test_month <<- get_month(3)
model_alias_modeling_vector <- c()

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


# # set random seed for reproducibility
set.seed(5)

cores <- detectCores() - 1

searchGridSubCol <- expand.grid(
  max_depth = c(4, 6,12),
  eta = c(0.1, 0.3, 0.6, 0.8),
  gamma = c(0),
  alpha = c(1),
  early_stoping_round = c(20),
  nrounds = c(1000)
)


for(i in 7:length(products)){
  
  model_alias_modeling <<-
    paste0("tesis", today() %>% format(., "%Y%m%d"), "_", products[i])
  model_alias_modeling_vector[i] <-
    paste0("tesis", today() %>% format(., "%Y%m%d"), "_", products[i])
  model_type_modeling <<- products[i]
  
  dt_product_path <- os.path.join(data_tesis_path, products[i]) 
  
  # load data
  print(paste("Loading data for", products[i]))
  data <- get.path(dt_product_path, products[i]) %>% readRDS()
  data[, aa_estrato := paste0("estr_", aa_estrato)]
  data[, aa_estrato := factor(aa_estrato, levels = paste0("estr_", 0:6))]
  
  # divinding master table
  test <- data[periodo == test_cut]
  dev <- data[periodo == dev_cut]
  master <- data[periodo >= train_cut_min &
                   periodo <= train_cut_max]
  
  # create model 
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
  
  numeric_cols <-
    c(crm_vars[!(sapply(master[, mget(crm_vars)], is.factor))],
      products_variables)
  
  print("One hot encoding")
  # one-hot encode the categorical features
  final_cols <- c("target",categorical_cols, numeric_cols)
  
  master_dmatrix <-
    sparse.model.matrix(target ~ . - 1, data = master[, mget(final_cols)])
  
  dev_dmatrix <-
    sparse.model.matrix(target ~ . - 1, data = dev[, mget(final_cols)])
  test_dmatrix <-
    sparse.model.matrix(target ~ . - 1, data = test[, mget(final_cols)])
  
  model_cols <- test_dmatrix@Dimnames[[2]]
  
  # separate target
  target_train_dmatrix <-
    as(data.matrix(master$target), 'dgCMatrix')
  target_dev_dmatrix <-
    as(data.matrix(dev$target), 'dgCMatrix')
  target_test_dmatrix <-
    as(data.matrix(test$target), 'dgCMatrix')
  
  dtrain <-
    xgb.DMatrix(data = master_dmatrix, label = target_train_dmatrix)
  ddev <- xgb.DMatrix(data = dev_dmatrix, label = target_dev_dmatrix)
  dtest <- xgb.DMatrix(data = test_dmatrix, label = target_test_dmatrix)

  gc()
  
  watchlist <- list(train = dtrain, test = ddev)

  #Tuning de parametros
  
  print("Tuning parameters")
  
  system.time(ErrorsHyperparameters <-
                apply(searchGridSubCol, 1, function(parameterList) {
                  current_max_depth <- parameterList[["max_depth"]]
                  current_eta <- parameterList[["eta"]]
                  current_gamma <- parameterList[["gamma"]]
                  current_alpha <- parameterList[["alpha"]]
                  current_early_stoping_round <-
                    parameterList[["early_stoping_round"]]
                  current_nrounds <- parameterList[["nrounds"]]
                  
                  xgb.parameters_tuning <- list(
                    booster = "gbtree",
                    objective = "binary:logistic",
                    max.depth = current_max_depth,
                    eta = current_eta,
                    gamma = current_gamma,
                    eval_metric = "aucpr",
                    alpha = current_alpha,
                    early_stoping_round = current_early_stoping_round,
                    nrounds = current_nrounds
                  )
                  
                  model_tuning <- xgb.train(
                    data = dtrain,
                    nround = xgb.parameters_tuning$nrounds,
                    params = xgb.parameters_tuning,
                    early_stopping_rounds = xgb.parameters_tuning$early_stoping_round,
                    verbose = 1,
                    watchlist = watchlist,
                    nthread = cores
                  )
                  
                  xvalidationScores <- as.data.frame(model_tuning$evaluation_log)
                  xvalidationScores <- data.table(xvalidationScores)
                  xvalidationScores <- xvalidationScores[order(-test_aucpr)]
                  
                  testaucpr <- xvalidationScores$test_aucpr[1]
                  trainaucpr <- xvalidationScores$train_aucpr[1]
                  
                  
                  output <-
                    return(
                      c(
                        testaucpr,
                        trainaucpr,
                        current_max_depth,
                        current_eta,
                        current_gamma,
                        current_alpha,
                        current_early_stoping_round,
                        current_nrounds
                      )
                    )
                }
                )
  )
  
  output <- as.data.frame(t(ErrorsHyperparameters))
  varnames <-
    c(
      "Testaucpr",
      "Trainaucpr",
      "max_depth",
      "eta",
      "gamma",
      "alpha",
      "early_stoping",
      "nrounds"
    )
  names(output) <- varnames
  
  output <- data.table(output)
  output <- output[order(-Testaucpr, -Trainaucpr)]
  
  print("Best 5 results:")
  print(head(output, 5))
  
  # training model
  print("Training xgboost model")
  
  xgb.parameters <- list(booster = "gbtree",
                         objective = "binary:logistic",
                         max.depth = output$max_depth[1],
                         eta = output$eta[1],
                         gamma = output$gamma[1],
                         eval_metric = "aucpr", 
                         alpha = output$alpha[1],
                         early_stoping_round = output$early_stoping[1],
                         nrounds = output$nrounds[1]) 
  
  model_xgb <- xgb.train(
    data = dtrain,
    nround = xgb.parameters$nrounds,
    params = xgb.parameters,
    early_stopping_rounds = xgb.parameters$early_stoping_round,
    verbose = 1 ,
    nthread = cores, 
    watchlist = watchlist
    
  )
  # saving objects 
  print("Saving models and tables")
  
  # create directory 
  print(paste("Creating directory", model_alias_modeling))
  model_alias_path <-
    os.path.join(models_path, model_alias_modeling)
  dir.create(model_alias_path)
  
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
  
  fwrite(data_id, os.path.join(model_alias_path, paste0(model_alias_modeling, "_id.csv")))
  xgb.save(model_xgb, os.path.join(model_alias_path, paste0(model_alias_modeling, ".model")))
  

  save(model_cols, master_dmatrix, dev_dmatrix, test_dmatrix, xgb.parameters , output,
       master, dev, test, 
       file = os.path.join(model_alias_path, paste0(products[i], "_objects.RData")))
}



