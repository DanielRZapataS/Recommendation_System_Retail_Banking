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
set_environment() 

# Load configuration file and values
config <- fromJSON("settings.json")
products <<- c("tcredito", "crediservice", "ahorros", "cdt", "vehiculo", 
               "libranza", "libredestino", "nomina", "vivienda")



# Second step: Training to generate new models  
job <<- "Training"
model_alias_modeling_vector <- c()
train_months <<- c("201708", get_month(7))
dev_month <<- get_month(6)
test_month <<- get_month(5)

for(i in 2:length(products)){
  model_alias_modeling <<-
    paste0("tesis_", today() %>% format(., "%Y%m%d"), "_", products[i])
  model_alias_modeling_vector[i] <-
    paste0("tesis_", today() %>% format(., "%Y%m%d"), "_", products[i])
  model_type_modeling <<- products[i]
  source(os.path.join("scripts", "orquestador.R"))
  gc()
} 
