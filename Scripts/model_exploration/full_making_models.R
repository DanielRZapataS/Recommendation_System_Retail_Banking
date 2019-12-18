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

# products <<- c( "crediservice", "tcredito", "ahorros", "cdt",  
#                "libranza", "libredestino", "nomina", "vivienda")

products <<- c("libredestino")

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

source("Scripts/model_exploration/model_selection.R")
for(i in 1:length(products)){
  
model_selection(data_tesis_path,
                products,
                train_cut_max,
                train_cut_min,
                dev_cut,
                i)
}










