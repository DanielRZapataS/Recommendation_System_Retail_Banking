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
library(MLmetrics)
library(ROCR)
library(Ckmeans.1d.dp)
library(xgboostExplainer)
library(rsvg)
library(DiagrammeR)
library(pROC)
library(caret)

data_tesis_path <- os.path.join(data_path, "Temporary")

products <<- c("tcredito", "crediservice", "ahorros", "cdt", "vehiculo", 
               "libranza", "libredestino", "nomina", "vivienda")

i = 1

# load data
print(paste("Loading data for", products[i]))
dt_product_path <- os.path.join(data_tesis_path, products[i]) 
data <- get.path(dt_product_path, products[i]) %>% readRDS()
data[, aa_estrato := paste0("estr_", aa_estrato)]
data[, aa_estrato := factor(aa_estrato, levels = paste0("estr_", 0:6))]

#load model 

model_path <- get.path(models_path, products[i])
model_xgb <- get.path(model_path, "model") %>%  xgb.load()
model_id <- get.path(model_path, "id") %>% fread()
load(get.path(model_path, "objects"))


# converting cutting months
train_months <-
  c(model_id[var == "train_months_since", value], model_id[var == "train_months_to", value])
dev_month <- model_id[var == "dev_month", value]
test_month <- model_id[var == "test_month", value]

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
test <- data[periodo == test_cut]
dev <- data[periodo == dev_cut]
master <- data[periodo >= train_cut_min &
                 periodo <= train_cut_max] 


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


## plot tree

xgb.plot.tree(model = model_xgb,  trees = 0, show_node_id = TRUE)
gr <- xgb.plot.tree(model= model_xgb,  trees = 1, render=FALSE, show_node_id = TRUE)
export_graph(gr, 'Plots/tree.pdf', width=1500, height=1900)


# • Features names of the features used in the model;

# • Gain represents fractional contribution of each feature to the model based on the total gain of
# this feature’s splits. Higher percentage means a more important predictive feature.
# Gain. The average training loss reduction gained when using a feature for splitting.


# • Cover metric of the number of observation related to this feature;
# Cover. The number of times a feature is used to split the data across all trees weighted by the number of training data points that go through those splits.

# • Frequency percentage representing the relative number of times a feature have been used in
# trees.


importance_matrix <-
  xgb.importance(feature_names = model_cols, model = model_xgb)

gg <- xgb.ggplot.importance(importance_matrix, measure = "Gain", rel_to_first = FALSE, top_n = 20)
gg + ggplot2::ylab("Gain")

gg <- xgb.ggplot.importance(importance_matrix, measure = "Cover", rel_to_first = FALSE, top_n = 20)
gg + ggplot2::ylab("Cover")

gg <- xgb.ggplot.importance(importance_matrix, measure = "Frequency", rel_to_first = FALSE, top_n = 20)
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







