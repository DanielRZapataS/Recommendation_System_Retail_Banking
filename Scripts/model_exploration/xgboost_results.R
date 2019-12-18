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


xgb.attributes(model_xgb)

########################
###### Predictions #####
########################
xgb_preds_master <-  predict(model_xgb, master_dmatrix)
xgb_preds_dev <-  predict(model_xgb, dev_dmatrix)
xgb_preds_test <-  predict(model_xgb, test_dmatrix)

########################
#######  AUC   ########
########################
xgb_roc_obj_master <- roc(master$target, xgb_preds_master)
xgb_roc_obj_dev <- roc(dev$target, xgb_preds_dev)
xgb_roc_obj_test <- roc(test$target, xgb_preds_test)

paste("XGB ROC AUC train", pROC::auc(xgb_roc_obj_master))
paste("XGB ROC AUC develop", pROC::auc(xgb_roc_obj_dev))
paste("XGB ROC AUC test", pROC::auc(xgb_roc_obj_test))

# using ML metrics package 
xgb_pr_auc_master <- PRAUC(xgb_preds_master, master$target)
xgb_pr_auc_dev <- PRAUC(xgb_preds_dev, dev$target)
xgb_pr_auc_test <- PRAUC(xgb_preds_test, test$target)

paste("XGB PR AUC train", xgb_pr_auc_master)
paste("XGB PR AUC develop", xgb_pr_auc_dev)
paste("XGB PR AUC test", xgb_pr_auc_test)

# PR auc are not consistent with xgboost train

# using ROCR package

xgb_pred_master_rocr <- prediction(xgb_preds_master, master$target)
xgb_pred_dev_rocr <- prediction(xgb_preds_dev, dev$target)
xgb_pred_test_rocr <- prediction(xgb_preds_test, test$target)

# ROC AUC
xgb_perf_master_rocr <- performance(xgb_pred_master_rocr, "tpr", "fpr")
xgb_perf_dev_rocr <- performance(xgb_pred_dev_rocr, "tpr", "fpr")
xgb_perf_test_rocr <- performance(xgb_pred_test_rocr, "tpr", "fpr")

plot(xgb_perf_master_rocr)
plot(xgb_perf_dev_rocr)
plot(xgb_perf_test_rocr)

# Precision recall auc 
xgb_perf_master_prAUC <- performance(xgb_pred_master_rocr, "prec", "rec")
xgb_perf_dev_prAUC <- performance(xgb_pred_dev_rocr, "prec", "rec")
xgb_perf_test_prAUC <- performance(xgb_pred_test_rocr, "prec", "rec")

plot(xgb_perf_master_prAUC)
plot(xgb_perf_dev_prAUC)
plot(xgb_perf_test_prAUC)



# get threshold from roc auc curves 
# threshold is calculated using develop data 
pUmbral_dev <- coords(xgb_roc_obj_dev, "best", ret = "threshold")
errorClasificacion_dev <-
  misClassError(dev$target, xgb_preds_dev, threshold = pUmbral_dev)

predictedClass_master <- ifelse(xgb_preds_master >= pUmbral_dev,1,0)
predictedClass_dev <- ifelse(xgb_preds_dev >= pUmbral_dev,1,0)
predictedClass_test <- ifelse(xgb_preds_test >= pUmbral_dev,1,0)

matrizConfusion_dev <-
  caret::confusionMatrix(
    data = as.factor(predictedClass_dev),
    reference = as.factor(dev$target)
  )
matrizConfusion_dev

errorClasificacion_test <-
  misClassError(test$target, xgb_preds_test, threshold = pUmbral_dev)
matrizConfusion_test <-
  caret::confusionMatrix(
    data = as.factor(predictedClass_test),
    reference = as.factor(test$target)
  )
matrizConfusion_test


##############################
####### ACCURACY #######
##############################
# the proportion of elements in actual that are equal to the corresponding
# element in predicted
xgb_accuracy_master <- accuracy(master$target,predictedClass_master)
xgb_accuracy_dev <- accuracy(dev$target,predictedClass_dev)
xgb_accuracy_test <- accuracy(test$target,predictedClass_test)

paste("XGB ACCURACY train", xgb_accuracy_master)
paste("XGB ACCURACY develop", xgb_accuracy_dev)
paste("XGB ACCURACY test", xgb_accuracy_test)

########################
####### PRECISON #######
########################
# precision computes proportion of observations predicted to be in the positive
# class (i.e. the element in predicted equals 1) that actually belong to the
# positive class (i.e. the element in actual equals 1)
xgb_precision_master <- Metrics::precision(master$target, predictedClass_master)
xgb_precision_dev <- Metrics::precision(dev$target, predictedClass_dev)
xgb_precision_test <- Metrics::precision(test$target, predictedClass_test)

paste("XGB precision train", xgb_precision_master)
paste("XGB precision develop", xgb_precision_dev)
paste("XGB precision test", xgb_precision_test)

######################
####### RECALL #######
######################
# recall computes proportion of observations in the positive class (i.e. the
# element in actual equals 1) that are predicted to be in the positive class
# (i.e. the element in predicted equals 1)
xgb_recall_master <- Metrics::recall(master$target, predictedClass_master)
xgb_recall_dev <- Metrics::recall(dev$target, predictedClass_dev)
xgb_recall_test <- Metrics::recall(test$target, predictedClass_test)

paste("XGB recall train", xgb_recall_master)
paste("XGB recall develop", xgb_recall_dev)
paste("XGB recall test", xgb_recall_test)



