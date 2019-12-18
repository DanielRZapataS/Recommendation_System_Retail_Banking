### getting tesis results ####

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
results_folder <- os.path.join(results_path, "rentabilidad")
dir.create(results_folder)


# productos

ahorros <- fread("//bdbemcfs.bancodebogota.net/Analytics/Informacion_Centralizada/Data_Analytics/Detalle_Productos/AHORROS_2018_12.csv")

cdt <- fread("//bdbemcfs.bancodebogota.net/Analytics/Informacion_Centralizada/Data_Analytics/Detalle_Productos/CDT_2018_12.csv")

saldos_mora <- fread("//bdbemcfs.bancodebogota.net/Analytics/Informacion_Centralizada/Data_Analytics/Saldos_Moras/Activo_Saldos_Moras_2018.csv")

#diccionarios 

rentabilidad <- fread("Data/rentabilidad/rentabilidad.csv")
lineas_credito <- fread("Data/rentabilidad/codigos_lineas_credito.csv")

#####

saldos_mora <- saldos_mora[PERIODO == 201812 & SARC_TIPO_IDENTIFICACION == "CC", .(SARC_LINEA_CREDITO, SARC_SALDO_CAPITAL)]

saldos_mora <- merge(saldos_mora, lineas_credito[, .(codigo, producto)], by.x = "SARC_LINEA_CREDITO", by.y = "codigo" )

productos <- c("pr_libredestino", 
               "pr_crediservice",
               "pr_vivienda",
               "pr_libranza",
               "pr_tarjeta_credito")
saldos_mora <- saldos_mora[producto %in% productos]

rentabilidad_activo <- saldos_mora[, .(saldo_promedio = mean(SARC_SALDO_CAPITAL, na.rm = T)), by = producto]


ahorros[, producto := ifelse(CODIGO_SUBPRODUCTO == 10, "pr_nomina", "pr_ahorros")]
ahorros <- ahorros[TIPO_ID == "C", .(PROMEDIO_MES, producto)]

rentabilidad_ahorros <- ahorros[, .(saldo_promedio = mean(PROMEDIO_MES, na.rm = T)), by = producto]                 


rentabilidad <- merge(rbindlist(list(rentabilidad_activo, rentabilidad_ahorros)), rentabilidad, by = "producto")
rentabilidad[producto == "pr_tarjeta_credito", 
                    producto := "pr_tcredito"]
rentabilidad[, rentabilidad := saldo_promedio*ROA]

fwrite(rentabilidad,
       os.path.join(results_folder, "rentabilidad_productos.csv"))

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

# compradores modelo 

uplift <- matrix(NA, nrow = length(products), ncol = 2) 
colnames(uplift) <- c("producto", "compradores_modelo")
uplift[, 1] <- products

for(i in 1:length(final_models)){
  model_path <- os.path.join(models_path, final_models[i])
  aux <- get.path(model_path, "test_uplift") %>% fread()
  uplift[i, 2] <- aux[porcentajesAnalizados == 10, totalCompradoresAcumulados]
 
}
uplift <- data.table(uplift)
uplift[, compradores_modelo := as.numeric(compradores_modelo)]
### compradores banco 

pred_npb <- get.path(results_folder, "pred_npb") %>% fread()
targets <- grep("target", names(pred_npb), value = T)

set.seed(1)
clientes <- sample(pred_npb$llave, length(pred_npb$llave)*0.1)

base <- pred_npb[llave %in% clientes, lapply(.SD, sum), .SDcols = targets]


names(base) <-  gsub("target_", "", names(base))

base <- melt(base)
names(base) <- c("producto", "compradores_banco")

rentabilidad[, producto := substr(producto, 4, 20)]

analisis_rentabilidad <- merge(rentabilidad, uplift, by = "producto")
analisis_rentabilidad <- merge(analisis_rentabilidad, base, by = "producto")  
analisis_rentabilidad[, rent_modelo := compradores_modelo*rentabilidad]  
analisis_rentabilidad[, rent_banco := compradores_banco*rentabilidad]    
analisis_rentabilidad[, comp_rent := (rent_modelo - rent_banco)/rent_banco ]

fwrite(analisis_rentabilidad, os.path.join(results_folder, "analisis_rentabilidad.csv"))



