# Metricas for propensity model

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

pred_npb <- fread("Results/Performance/201808/20181130_npb/pred_npb.csv")

products <- names(pred_npb)[str_detect(names(pred_npb), "target")]
products <- sapply(strsplit(products, "_"), "[[", 2)

staging <- get.path(staging_path, "201808") %>% readRDS()

##############################
##### K popular products #####
############################## 
# Get products recomendation recomending such popular products that clients do
# not have. Recomendation order depends on popular products vector.

# Popular products 
pr_products <- names(staging)[str_detect(names(staging), "pr_")]
popular_products <- staging[, lapply(.SD, sum), .SDcols = pr_products]
cut_names <- sapply(strsplit(names(popular_products), "_"), "[[", 2)
names(popular_products) <- cut_names
popular_products <- popular_products[, mget(products)]
popular_products <- melt(popular_products)
popular_products <- popular_products[order(-value), variable]

# Get clients portafolio of clients with products
clients_portafolio <- staging[, mget(c("llave",
                                       paste0("pr_", popular_products)))]
names(clients_portafolio)[-1] <-
  sapply(strsplit(names(clients_portafolio)[-1], "_"), "[[", 2)

clients_portafolio  <- melt(clients_portafolio , id.vars = c("llave"))
setkey(clients_portafolio, "llave")
clients_portafolio <- clients_portafolio[value > 0]
clients <- unique(clients_portafolio$llave)

# Getting products added 
target <-
  pred_npb[llave %in% clients, mget(c("llave", paste0("target_", products)))]
names(target)[-1] <-
  sapply(strsplit(names(target)[-1], "_"), "[[", 2)
target <- melt(target, id.vars = "llave")
target <- target[value > 0]
setkey(target, "llave")
clients_buyes <- unique(target$llave)

products_added <- list()
for(i in 1:length(clients_buyes)){
  products_added[[i]] <- target[llave == clients_buyes[i], variable]
}
names(products_added) <- clients_buyes
head(products_added)
saveRDS(products_added, "Tesis/products_added.rds")

# Get clients portafolio of clients with products
clients_portafolio <- staging[, mget(c("llave",
                                       paste0("pr_", popular_products)))]
clients_portafolio <- clients_portafolio[llave %in% clients_buyes]
names(clients_portafolio)[-1] <-
  sapply(strsplit(names(clients_portafolio)[-1], "_"), "[[", 2)

clients_portafolio  <- melt(clients_portafolio , id.vars = c("llave"))
setkey(clients_portafolio, "llave")
clients_portafolio <- clients_portafolio[value > 0]


# Get which popular produch client do not have 
products_recommender <- list()
for (i in 1:length(clients_buyes)) {
  products_recommender[[i]] <-
    popular_products[popular_products %!in% clients_portafolio[llave == clients_buyes[i], variable]]
}
names(products_recommender) <- clients_buyes
head(products_recommender)
saveRDS(products_recommender, "Tesis/products_recommender.rds")


## Adding clients that do not agregate products
loss_clients <- clients[clients %!in% clients_buyes]
products_added_loss <- list()
for(i in 1:length(loss_clients)){
  products_added_loss[[i]] <- ""
}

products_recommender_loss <- list()
for(i in 1:length(loss_clients)) {
  products_recommender_loss[[i]] <- products
}

#####################
###### METRICS ######
#####################

#####################
####### MAP@K #######
##################### 

predicted <- c(products_recommender, products_recommender_loss)
actual <- c(products_added, products_added_loss)

for(i in 1:9){
  print(mapk(i, actual, predicted))
}

#########################
# change matrix results #
#########################

clients_matrix <- pred_npb[llave %in% clients_buyes, mget(c("llave", paste0("target_", products)))]
names(clients_matrix)[-1] <-
  sapply(strsplit(names(clients_matrix)[-1], "_"), "[[", 2)

clients_matrix <- melt(clients_matrix, id.vars = c("llave"))

# clients that did not aggregate products
clients_loss_matrix <- data.table(llave = loss_clients, 
                                  ahorros = 0,
                                  cdt = 0, 
                                  crediservice = 0,
                                  libranza = 0,
                                  libredestino = 0,
                                  tcredito = 0,
                                  vehiculo = 0,
                                  nomina = 0, 
                                  vivienda = 0
)

clients_loss_matrix <- melt(clients_loss_matrix, id.vars = "llave")

clients_matrix <- rbindlist(list(clients_matrix, clients_loss_matrix))

# recomender 
clients_recommender_matrix <- list()
for(i in 1:8){
  clients_recommender_matrix[[i]] <- data.table(llave = clients_buyes)
}
for(j in 1:8){
  for (i in 1:length(clients_buyes)) {
    recomendation <- products_recommender[[i]]
    recomendation <- recomendation[1:j]
    clients_recommender_matrix[[j]][llave == clients_buyes[i], ahorros := ifelse("ahorros" %in% recomendation, 1, 0)]
    clients_recommender_matrix[[j]][llave == clients_buyes[i], cdt := ifelse("cdt" %in% recomendation, 1, 0)]
    clients_recommender_matrix[[j]][llave == clients_buyes[i], crediservice := ifelse("crediservice" %in% recomendation, 1, 0)]
    clients_recommender_matrix[[j]][llave == clients_buyes[i], libranza := ifelse("libranza" %in% recomendation, 1, 0)]
    clients_recommender_matrix[[j]][llave == clients_buyes[i], libredestino := ifelse("libredestino" %in% recomendation, 1, 0)]
    clients_recommender_matrix[[j]][llave == clients_buyes[i], tcredito := ifelse("tcredito" %in% recomendation, 1, 0)]
    clients_recommender_matrix[[j]][llave == clients_buyes[i], vehiculo := ifelse("vehiculo" %in% recomendation, 1, 0)]
    clients_recommender_matrix[[j]][llave == clients_buyes[i], nomina := ifelse("nomina" %in% recomendation, 1, 0)]
    clients_recommender_matrix[[j]][llave == clients_buyes[i], vivienda := ifelse("vivienda" %in% recomendation, 1, 0)]
  }
}

k = 1
for(k in 1:8){
  print(paste0("K = ", k))
  clients_recommender <-
    melt(clients_recommender_matrix[[k]], id.vars = c("llave"))

  # clients that did not aggregate products
  clients_recommender_loss_matrix <- data.table(llave = loss_clients, 
                                                ahorros = 1,
                                                cdt = 1, 
                                                crediservice = 1,
                                                libranza = 1,
                                                libredestino = 1,
                                                tcredito = 1,
                                                vehiculo = 1,
                                                nomina = 1, 
                                                vivienda = 1
  )
  
  clients_recommender_loss_matrix <-
    melt(clients_recommender_loss_matrix, id.vars = "llave")
  
  clients_recommender <-
    rbindlist(list(clients_recommender, clients_recommender_loss_matrix))
  
  
  ## merging 
  
  clients_compare <- merge(clients_matrix, clients_recommender, by = c("llave", "variable"))
  names(clients_compare)[3:4] <- c("actual", "predicted")
  
  ##############################
  ####### ACCURACY #######
  ##############################
  # the proportion of elements in actual that are equal to the corresponding
  # element in predicted
 print( accuracy(actual = clients_compare$actual, predicted = clients_compare$predicted))
  
  
  ########################
  ####### PRECISON #######
  ########################
  # precision computes proportion of observations predicted to be in the positive
  # class (i.e. the element in predicted equals 1) that actually belong to the
  # positive class (i.e. the element in actual equals 1)
 print( Metrics::precision(actual = clients_compare$actual, predicted = clients_compare$predicted))
  
  ######################
  ####### RECALL #######
  ######################
  # recall computes proportion of observations in the positive class (i.e. the
  # element in actual equals 1) that are predicted to be in the positive class
  # (i.e. the element in predicted equals 1)
 print( Metrics::recall(actual = clients_compare$actual, predicted = clients_compare$predicted))
  
}











