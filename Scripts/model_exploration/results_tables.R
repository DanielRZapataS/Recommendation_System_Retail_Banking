##### getting tesis results ####

##### starting ####
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

##### create results folder #### 
results_folder <- os.path.join(results_path, "tesis_tables_corregidas")
dir.create(results_folder)

##### final models folders ####
final_models <- list.files(models_path) %>% grep(pattern = "rf", value = T)
final_models <-
  c(final_models,
    "xgb_model_20190428_libranza",
    "xgb_model_20190428_vivienda")
# libranza's product has two models, the selected model is xgb
final_models <- final_models[final_models %!in% "rf_model_20190429_libranza"]

##### results by products #####
products <-  sapply(str_extract_all(final_models, "[a-z]+"), "[[", 3) 
# load test resuls by products
test_results <- list()
for(i in 1:length(final_models)){
  model_path <- os.path.join(models_path, final_models[i])
  aux <- get.path(model_path, "metrics_test") %>% fread()
  test_results[[i]] <- aux$x
  names(test_results[[i]]) <- aux$V1
  
}
names(test_results) <- products

# test_results <- rbindlist(test_results)
test_results <- data.frame(matrix(unlist(test_results), ncol=length(test_results), byrow=F))
row.names(test_results) <- c(
  "Exactitud",
  "Precisión",
  "Exhaustividad",
  "AUC",
  "Uplift al 10\\%",
  "Uplift al 20\\%"
)
names(test_results) <-
  c(
    "Ahorro",
    "Crediservice",
    "Tarjeta de crédito",
    "CDT",
    "Libre destino",
    "Nómina",
    "Libranza",
    "Vivienda"
  )
write.csv(round(test_results, 2),
          file = os.path.join(results_folder, "producto_test_results.csv"))

### hiper parametres model ####

# xgb
xgb_models <- grep(final_models, pattern = "xgb", value = T)

models_parameters <- list()
for(i in 1:length(xgb_models)){
  model_path <- os.path.join(models_path, xgb_models[i])
  aux <- get.path(model_path, "model_parameters") %>% fread()
  models_parameters[[i]] <- aux
}
models_parameters <-
  data.frame(matrix(
    unlist(models_parameters),
    nrow = length(models_parameters),
    byrow = T
  ))

names(models_parameters) <-
  c(
    "AUC Validación",
    "AUC Entrenamiento",
    "Profundidad máxima",
    "Eta",
    "Gamma",
    "Interrupción temprana",
    "Iteraciones",
    "Mejor iteración",
    "Número de variables"
  )
row.names(models_parameters) <- c("Libranza", "Vivienda")

write.csv(round(models_parameters, 2),
          file = os.path.join(results_folder, "xgb_best_parameters.csv"))

# random forest 
rf_models <- grep(final_models, pattern = "rf", value = T)

models_parameters <- list()
for(i in 1:length(rf_models)){
  model_path <- os.path.join(models_path, rf_models[i])
  best_parameters <- get.path(model_path, "best") %>% readRDS()
  grid_perf <- get.path(model_path, "rf_grid") %>% readRDS()
  models_parameters[[i]] <- as.numeric(c(best_parameters$number_of_trees, 
                              best_parameters$max_depth,
                              best_parameters$min_leaves,
                              best_parameters$max_leaves,
                              grid_perf$sample_rate[1],
                              grid_perf$auc[1]))
}
models_parameters <-
  data.frame(matrix(
    unlist(models_parameters),
    nrow = length(models_parameters),
    byrow = T
  ))

row.names(models_parameters) <- 
  c(
    "Ahorros",
    "Crediservice",
    "Tarjeta de crédito",
    "CDT",
    "Libre destino",
    "Nómina"
  )
names(models_parameters) <- c(
  "Número de árboles",
  "Profundidad máxima",
  "Minimas hojas",
  "Máximas hojas",
  "Tasa de muestra",
  "AUC validación"
)
write.csv(round(models_parameters, 2),
          file = os.path.join(results_folder, "rf_best_parameters.csv"))

###### results by client ####

for(i in 1:length(final_models)){
  model_path <- os.path.join(models_path, final_models[i])
  pred_test <- get.path(path = model_path, "pred_test") %>% fread()
  names_vector <- c("llave",
                    "periodo",
                    paste0("target_", products[i]),
                    paste0("pred_", products[i]),
                    paste0("pred_num_", products[i]))
  names(pred_test) <- names_vector
  if(i == 1){
    pred_npb <- copy(pred_test)
  }
  if(i != 1){
    pred_npb <- merge(pred_npb, pred_test, by = c("llave", "periodo"))
  }
}
fwrite(pred_npb, file = os.path.join(results_folder, "pred_npb.csv"))

targets <- names(pred_npb)[grepl("target_", names(pred_npb))]

#### Perfect MAP@K ####
# recomend every first product each client bought
pred_npb[, count := rowSums(pred_npb[, mget(targets)])]
perfect_mapk <-
  pred_npb[, .(.N, paste0(round((
    .N / nrow(pred_npb) * 100
  ), 2), "%")), by = count][order(-N)]
names(perfect_mapk) <-
  c("Productos Agregados",
    "Número de clientes",
    "Porcentaje de clientes")
fwrite(perfect_mapk, file = os.path.join(results_folder, "perfect_mapk.csv"))

perfect_mapk <-
  data.frame(
    perfect_mapk = sum(
      perfect_mapk$`Productos Agregados` * perfect_mapk$`Número de clientes`
    ) / sum(perfect_mapk$`Número de clientes`)
  )
fwrite(perfect_mapk, file = os.path.join(results_folder, "perfect_mapk_cifra.csv"))

# pred_npb[count > 0, .N]/nrow(pred_npb)
# pred_npb[count > 1, .N]/nrow(pred_npb)
# pred_npb[count > 2, .N]/nrow(pred_npb)
# pred_npb[count > 3, .N]/nrow(pred_npb)
# pred_npb[count > 4, .N]/nrow(pred_npb)

##### Next product to buy #### 

# getting clients who aggegate any product 
cols <- c("llave", targets)
products_acq <- pred_npb[, mget(cols)]
names(products_acq)[-1] <- products
products_acq  <- melt(products_acq , id.vars = c("llave"))
products_acq <- products_acq[products_acq$value > 0]
setkey(products_acq, "llave")
# products_acq[llave == "631322"]
clients_adq <- unique(products_acq$llave) 

products_acq_list <- list()
#making list 
for(i in 1:length(clients_adq)){
  products_acq_list[[i]] <- products_acq[llave == clients_adq[i], variable]
}
names(products_acq_list) <- clients_adq

# product propencity matrix 
probs <- names(pred_npb)[grepl("pred_" , names(pred_npb))]
probs <- probs[probs %in% paste0("pred_", products)]
cols <- c("llave", probs)
products_pred <- pred_npb[, mget(cols)]
names(products_pred)[-1] <- products
products_pred  <- melt(products_pred , id.vars = c("llave"))
products_pred[, pred_order := rank(-value), by = .(llave) ]
setkey(products_pred, "llave", "pred_order")

#clients who don't agregate any product
loss_clients <-
  products_pred$llave[products_pred$llave %!in% products_acq$llave]
loss_clients <- unique(loss_clients)

# making list 
products_pred_cl <- copy(products_pred[llave %in% clients_adq])
products_pred_list <- list()
#making list 
for(i in 1:length(clients_adq)){
  products_pred_list[[i]] <- products_pred_cl[llave == clients_adq[i], variable]
}
names(products_pred_list) <- clients_adq

### compare 
length(products_acq_list) == length(products_pred_list)

## Adding clients that do not agregate products

products_acq_list_loss <- list()
for(i in 1:length(loss_clients)){
  products_acq_list_loss[[i]] <- ""
}

products_pred_list_loss <- list()
for(i in 1:length(loss_clients)) {
  products_pred_list_loss[[i]] <- products
}


length(products_acq_list_loss) == length(products_pred_list_loss)


### calculate MAPK for next product to buy ####

actual <- c(products_acq_list, products_acq_list_loss)
predicted <- c(products_pred_list, products_pred_list_loss)

length(actual)
length(actual) == length(predicted)
mapk_results <- c()
for(i in 1:9){
  mapk_results[i] <- mapk(i, actual, predicted)
  print(mapk_results[i])
}
mapk_results

##### calculate mtrics for the next product to buy #####
response <- melt(pred_npb[, mget(paste0("target_", products))])
predictor <- melt(pred_npb[, mget(paste0("pred_num_", products))])


metrics_npb <-
  data.frame("Propensión de productos" = c(
    accuracy(response$value , predictor$value),
    Metrics::precision(response$value, predictor$value),
    Metrics::recall(response$value, predictor$value),
    Metrics::auc(response$value, predictor$value)
  ))
row.names(metrics_npb) <- c(
  "Exactitud",
  "Precisión",
  "Exhaustividad",
  "AUC"
)

###### k popular products #### 

# Get products recomendation recomending such popular products that clients do
# not have. Recomendation order depends on popular products vector.

# Popular products 
data_tesis_path <- os.path.join(data_path, "Temporary")

staging <-
  get.path(os.path.join(data_tesis_path, products[1]), "master") %>% readRDS()

pr_products <- paste0("pr_", products)
popular_products <- staging[, lapply(.SD, sum), .SDcols = pr_products]
names(popular_products) <- products
popular_products <- melt(popular_products)
popular_products <- popular_products[order(-value), variable]

# # Get clients portafolio of clients with products
clients_portafolio <- staging[periodo == "2018-10-01", mget(c("llave",
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

# Get clients portafolio of clients with products
clients_portafolio <- staging[periodo == "2018-10-01", mget(c("llave",
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

# recomend k popular products that clients do not have 

predicted <- c(products_recommender, products_recommender_loss)
actual <- c(products_added, products_added_loss)

mapk_results_k_popular1 <- c()
for(i in 1:9){
  mapk_results_k_popular1[i] <- mapk(i, actual, predicted)
  print(mapk_results_k_popular1[i])
}

#
products_recommender2 <- list()
for (i in 1:length(clients_buyes)) {
  products_recommender2[[i]] <-
    popular_products
}
names(products_recommender2) <- clients_buyes
head(products_recommender2)

# recomend k popular products that clients do not have 

predicted <- c(products_recommender2, products_recommender_loss)
actual <- c(products_added, products_added_loss)

mapk_results_k_popular2 <- c()
for(i in 1:9){
  mapk_results_k_popular2[i] <- mapk(i, actual, predicted)
  print(mapk_results_k_popular2[i])
}

### other matrix ####

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
                                  # vehiculo = 0,
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
    # clients_recommender_matrix[[j]][llave == clients_buyes[i], vehiculo := ifelse("vehiculo" %in% recomendation, 1, 0)]
    clients_recommender_matrix[[j]][llave == clients_buyes[i], nomina := ifelse("nomina" %in% recomendation, 1, 0)]
    clients_recommender_matrix[[j]][llave == clients_buyes[i], vivienda := ifelse("vivienda" %in% recomendation, 1, 0)]
  }
}

k = 1
tabla <- matrix(nrow = 4, ncol = 8)
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
                                                # vehiculo = 1,
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
   tabla[1, k] <- accuracy(actual = clients_compare$actual, predicted = clients_compare$predicted)
  
  
  ########################
  ####### PRECISON #######
  ########################
  # precision computes proportion of observations predicted to be in the positive
  # class (i.e. the element in predicted equals 1) that actually belong to the
  # positive class (i.e. the element in actual equals 1)
  tabla[2, k] <- Metrics::precision(actual = clients_compare$actual, predicted = clients_compare$predicted)
  
  ######################
  ####### RECALL #######
  ######################
  # recall computes proportion of observations in the positive class (i.e. the
  # element in actual equals 1) that are predicted to be in the positive class
  # (i.e. the element in predicted equals 1)
  tabla[3, k] <-  Metrics::recall(actual = clients_compare$actual, predicted = clients_compare$predicted)
  
  tabla[4, k] <-  Metrics::auc(actual = clients_compare$actual, predicted = clients_compare$predicted)
  
}

### creating final tables ####

## mapk ###
tabla_mapk <- matrix(ncol = 9, nrow = 3)
tabla_mapk[1, ] <- mapk_results
tabla_mapk[2, ] <- mapk_results_k_popular1
tabla_mapk[3, ] <- mapk_results_k_popular2
tabla_mapk <- data.frame(tabla_mapk)
names(tabla_mapk) <- paste0("K = ", 1:9)
row.names(tabla_mapk) <-
  c("Propensión de productos", "K productos más populares personalizado", "K productos más populares")
write.csv(tabla_mapk, file = os.path.join(results_folder, "comparacion_mapk.csv"))

## metrics 
metrics_npb
tabla <- data.frame(tabla)
names(tabla) <- paste("K =", 1:8, "productos más populares personalizado")
tabla_metricas <- cbind(metrics_npb, tabla)
write.csv(tabla_metricas, file = os.path.join(results_folder, "comparacion_metricas.csv"))
