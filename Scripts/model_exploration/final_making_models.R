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
library(stringi)

# library(randomForest)
# h2o.no_progress()
h2o.init()

data_tesis_path <- os.path.join(data_path, "Temporary")

rf_products <<- c("crediservice", "tcredito", "ahorros", "cdt",  
                  "libranza", "libredestino", "nomina"  )

xgb_products <<- c( "libranza", "vivienda")

train_months <<- c(get_month(22), get_month(8))
dev_month <<- get_month(7)
test_month <<- get_month(6)

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


# set random seed for reproducibility
set.seed(5)

# source("Scripts/model_exploration/model_selection.R")
source("Scripts/model_exploration/xgb_tunnig_models.R")
source("Scripts/model_exploration/rf_model.R")
searchGridSubCol <- expand.grid(
  max_depth = c(4, 6,12),
  eta = c(0.1, 0.3,0.8),
  gamma = c(0),
  alpha = c(1),
  early_stoping_round = c(30),
  nrounds = c(1000)
)
# searchGridSubCol <- expand.grid(
#   max_depth = c(4, 6),
#   eta = c(0.1, 0.3 ),
#   gamma = c(0),
#   early_stoping_round = c(30),
#   nrounds = c(1000)
# )
# 

rf_params <- list(
  max_depth = c(3, 5, 10, 20),
  sample_rate = c(0.6320000291, 0.8)
)


for(i in 2:length(xgb_products)){
  xgb_tunning_models(
    data_tesis_path,
    xgb_products,
    train_cut_max,
    train_cut_min,
    dev_cut,
    searchGridSubCol,
    i
  )
}
  
rf_products <<- c("crediservice", "tcredito", "ahorros", "cdt",  
                  "libranza", "libredestino", "nomina"  )

i = 7
for(i in 6:length(rf_products)) {
  source("Scripts/utiles.R")
  set_environment()
  source("Scripts/model_exploration/rf_model.R")
  library(stringi)
  library(h2o)
  h2o.init()
  data_tesis_path <- os.path.join(data_path, "Temporary")
  
  rf_products <<- c("crediservice", "tcredito", "ahorros", "cdt",  
                    "libranza", "libredestino", "nomina"  )
  
  xgb_products <<- c( "libranza", "vivienda")
  
  train_months <<- c(get_month(22), get_month(8))
  dev_month <<- get_month(7)
  test_month <<- get_month(6)
  
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
  rf_params <- list(
    max_depth = c(3, 5, 10, 20),
    sample_rate = c(0.6320000291, 0.8)
  )
  
  rf_model(data_tesis_path,
           rf_products,
           train_cut_max,
           train_cut_min,
           dev_cut,
           rf_params,
           i)
  rm(list=ls())
  gc()
}










