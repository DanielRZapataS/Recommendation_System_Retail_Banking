## Starting

# Clean environment
rm(list = ls())
gc()
# Disable scientific notation
options(scipen = 999)
# Change prompt
options(prompt = "NPB_V0.3> ", continue = " ")

# Load utilities functions (change wd, auxiliary scripts...)
source("Scripts/utiles.R")
set_environment()
library(Matrix)
library(rsample)
library(dplyr)
library(h2o)
library(DALEX)
library(factorMerger)
library(breakDown)
library(cowplot)
library(scales)
library(ggcorrplot)
library(MASS)
library(gridExtra)
library(Metrics)
# library(randomForest)
# h2o.no_progress()
h2o.init()

data_tesis_path <- os.path.join(data_path, "Temporary")

products <<- c(
  "tcredito",
  "crediservice",
  "ahorros",
  "cdt",
  "libranza",
  "libredestino",
  "nomina",
  "vivienda"
)


# set random seed for reproducibility
set.seed(5)

cores <- detectCores() - 1

model_list <-
  grep("full_models", list.files(models_path), value = T)



for (i in model_list) {
  product <- substr(i, 22, 35)
  model_alias_modeling <- os.path.join(models_path, i)
  
  
  # load importance variable
  importance_matrix_xgb <- fread(os.path.join(model_alias_modeling, "xgb_auc_important_variables.csv"))
  
  importance_matrix_rf <- fread(os.path.join(model_alias_modeling, "rf_important_variables.csv"))
  
  # load vip
  # load(os.path.join(model_alias_modeling, paste0(product, "_vip.RData")))
  
  
  
  ##### selectin variables ####
  
  # selecting variables by permutations
  num_vars <- 40
  # vip_xgb_auc_table <- data.table(vip_xgb_auc)
  # vip_xgb_auc_table <- vip_xgb_auc_table[order(- dropout_loss)]
  # var_vip_xgb <-
  #   vip_xgb_auc_table[dropout_loss > 0.05, variable] %>% as.character()
  # var_vip_xgb <- var_vip_xgb[var_vip_xgb %!in% "_baseline_"]
  #
  # vip_rf_table <- data.table(vip_rf)
  # vip_rf_table <- vip_rf_table[order(- dropout_loss)]
  # var_vip_rf <- vip_rf_table[1:num_vars, variable]
  # var_vip_rf <- var_vip_rf[var_vip %!in% "_baseline_"]
  
  # selecting variables by xgboost gain
  var_gain <- importance_matrix_xgb[order(-Gain)]
  var_gain <- var_gain[Gain > 0.01, Feature]
  
  # selecting variables by xgboost cover
  var_cover <- importance_matrix_xgb[order(-Cover)]
  var_cover <- var_cover[Cover > 0.01, Feature]
  
  # selecting variables by xgboost frequency
  var_freq <- importance_matrix_xgb[order(-Frequency)]
  var_freq <- var_freq[Frequency > 0.01, Feature]
  
  # selecting variables by 
  imp_rf_vars <- importance_matrix_rf[order(-percentage)]
  imp_rf_vars <- imp_rf_vars[percentage > 0.01, variable]
  
  imp_xgb_vars <- c(var_gain, var_cover, var_freq) %>% unique
  imp_all_vars <- c(imp_xgb_vars, imp_rf_vars)%>% unique
  save(imp_xgb_vars, imp_all_vars,imp_rf_vars ,
          file = os.path.join(model_alias_modeling, paste0(product, "_important_variables.RData")))
  

}
