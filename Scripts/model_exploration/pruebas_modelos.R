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
# Set up paths

install.packages("devtools") 
library(devtools) 
install_github("AppliedDataSciencePartners/xgboostExplainer")

set_environment()

install.packages("Ckmeans.1d.dp")
install.packages("rsvg")
install.packages("DiagrammeR")
install.packages("pROC")
install.packages("caret")

library(Ckmeans.1d.dp)
library(xgboostExplainer)
library(rsvg)
library(DiagrammeR)
library(pROC)
library(caret)


products <<- c("tcredito", "crediservice", "ahorros", "cdt", "vehiculo", 
               "libranza", "libredestino", "nomina", "vivienda")

train_months <<- c(get_month(17), get_month(6))
dev_month <<- get_month(5)
test_month <<- get_month(4)
model_alias_modeling_vector <- c()

model_alias_modeling <<-
  paste0("tesis", today() %>% format(., "%Y%m%d"), "_", products[1])
model_alias_modeling_vector[1] <-
  paste0("tesis", today() %>% format(., "%Y%m%d"), "_", products[1])
model_type_modeling <<- products[1]


# Model 

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
  readRDS(get.path(feature_path,  get_month(2)))

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

## sampling

master <- master[sample(.N, .N*0.1)]
dev <- dev[sample(.N, .N*0.1)]
test <- test[sample(.N, .N*0.1)]


saveRDS(master, file = "Data/Temporary/master_exercise.rds")
saveRDS(dev, file = "Data/Temporary/dev_exercise.rds")
saveRDS(test, file = "Data/Temporary/test_exercise.rds")

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
# rm(master_dmatrix, target_train_dmatrix, dev_dmatrix, target_dev_dmatrix)
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

model_xgb <- xgb.train(
  data = dtrain,
  nround = xgb.parameters$nrounds,
  params = xgb.parameters,
  early_stopping_rounds = xgb.parameters$early_stoping_round,
  verbose = 1 ,
  nthread = cores, 
  watchlist = watchlist
  
)

## AUC 

xgb_preds_test = predict(model_xgb, test_dmatrix)
xgb_roc_obj_test <- roc(test$target, xgb_preds_test)



cat("XGB AUC test", auc(xgb_roc_obj_test))


## plot tree

xgb.plot.tree(model = model_xgb,  trees = 1, show_node_id = TRUE)
# gr <- xgb.plot.tree(model= model_xgb,  trees = 1, render=FALSE)
# export_graph(gr, 'Plots/tree.pdf', width=1500, height=1900)


# • Features names of the features used in the model;

# • Gain represents fractional contribution of each feature to the model based on the total gain of
# this feature’s splits. Higher percentage means a more important predictive feature.
# Gain. The average training loss reduction gained when using a feature for splitting.


# • Cover metric of the number of observation related to this feature;
# Cover. The number of times a feature is used to split the data across all trees weighted by the number of training data points that go through those splits.

# • Frequency percentage representing the relative number of times a feature have been used in
# trees.

cols <- c(ohe_cols, numeric_cols)
importance_matrix <-
  xgb.importance(feature_names = cols, model = model)

(gg <- xgb.ggplot.importance(importance_matrix, measure = "Gain", rel_to_first = FALSE, top_n = 20))
gg + ggplot2::ylab("Gain")

(gg <- xgb.ggplot.importance(importance_matrix, measure = "Cover", rel_to_first = FALSE, top_n = 20))
gg + ggplot2::ylab("Cover")

(gg <- xgb.ggplot.importance(importance_matrix, measure = "Frequency", rel_to_first = FALSE, top_n = 20))
gg + ggplot2::ylab("Frequency")

# SHAP values 

contr <- predict(model_xgb, dev_dmatrix, predcontrib = TRUE)
xgb.plot.shap(dev_dmatrix,
              contr,
              model = model_xgb,
              top = 12,
              n_col = 3)

## tree values
model_tre_xgb <- xgb.model.dt.tree(feature_names = cols, model = model_xgb)
 
# xgboost explainer 

explainer <-
  buildExplainer(
    model_xgb,
    dtrain,
    type = "binary",
    trees_idx = NULL
  )

pred.breakdown = explainPredictions(model_xgb, explainer, ddev)

cat('Breakdown Complete','\n')
weights = rowSums(pred.breakdown)
pred.xgb = 1/(1+exp(-weights))
cat(max(xgb.preds-pred.xgb),'\n')






