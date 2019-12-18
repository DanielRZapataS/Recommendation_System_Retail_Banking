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
library(scales)
library(GGally)
data_tesis_path <- os.path.join(data_path, "Temporary")
master <-  get.path(os.path.join(data_tesis_path, "ahorros"), "master") %>% readRDS()
head(master)

plots_dem <- "Plots/demograficas/demograficas"
dir.create(plots_dem)
# purchase.frequencies <- get.path(feature_path, "purchase") %>% readRDS()
# names(purchase.frequencies)

## Clientes por producto

products <<- c("tcredito", "crediservice", "ahorros", "cdt",  
               "libranza", "libredestino", "nomina", "vivienda")
products <- paste0("pr_", products)

plot_pr <- master[, lapply(.SD, sum), by = periodo, .SDcols = products ]
names(plot_pr)[-1] <- c("T crédito", "Crediservice", "Ahorros", "CDT", "Libranza", "Libredestino", "Nómina", "Vivienda")

plot_pr <- plot_pr[periodo <= "2018-12-01"]

plot_pr <- melt(plot_pr, id.vars = c("periodo"))
p1 <- plot_pr %>% ggplot(aes(
  x = periodo,
  y = value,
  color = variable,
  linetype = variable
)) +
  geom_line(size = 0.8, alpha = 0.7) +
  labs(color = "Productos", linetype = "Productos", title = "Evolución del número \n de unidades de los productos", x = "Periodo", y = "Total de unidades")+
  theme_minimal()

ggsave(filename = "Plots/demograficas/evolucion.png", p1)

plot_pr <-
  plot_pr[periodo == "2018-12-01"][order(-value)][, variable := factor(variable, ordered = T)]

p2 <- plot_pr %>% ggplot(aes(
  x = variable,
  y = value,
  fill= variable
)) +
  geom_bar(stat="identity") +
  scale_x_discrete(limits = plot_pr$variable) +
  geom_text(aes(label=value), vjust=1, color="white", size=3.5)+
  labs(fill = "Productos", title = "Total de unidades de productos en Diciembre", x = "Productos", y = "Total de unidades") +
  theme_minimal()
ggsave(filename = "Plots/demograficas/tenecia_octubre.png", p2) 

## conteo de productos 

master[, count := rowSums(master[, mget(products)])]
plot_count <- master[, .N, by = .(periodo, count)][order(-N)]
plot_count <- plot_count[count > 0]
plot_count[, count := ifelse(count >= 10, "Más de 10", count)]
plot_count <- plot_count[, sum(N), by = .(periodo, count)]

plot_count[, count := as.character(count)]

p3 <- plot_count %>% ggplot(aes(
  x = periodo,
  y = V1,
  color = count,
  linetype = count
)) +
  geom_line(size = 0.8, alpha = 0.7) +
  labs( colour = "Número de productos", linetype = "Número de productos", 
        title = "Evolución del número de unidades \n de productos por cliente", 
        x = "Periodo", y = "Total de clientes" ) +
  theme_minimal()

ggsave(filename = "Plots/demograficas/evolucion_tenecia.png", p3)

plot_count <-
  plot_count[periodo == "2018-12-01"][order(-V1)][, count := factor(count, ordered = T)]

p4 <- plot_count %>% ggplot(aes(
  x = count,
  y = V1,
  fill= count
)) +
  geom_bar(stat="identity") +
  scale_x_discrete(limits = plot_count$count) +
  labs( colour = "Número de productos", linetype = "Número de productos", 
        title = "Total de unidades de productos \n por cliente en Diciembre", 
        x = "Periodo", y = "Total de clientes" ) +
  geom_text(aes(label= comma(V1)), vjust=1, color="white", size=3.5) +
  theme_minimal()
ggsave(filename = "Plots/demograficas/evolucion_tenecia_octubre.png", p4) 

## graficas de la target 
master[, target := NULL]
products <- head(names(master)[grepl("pr_",names(master))], 9)
products <- products[products != "pr_vehiculo"]
for(i in 1:length(products)){
  print(paste0("Creating target variable ", products[i]))
  var_target <- products[i]
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
  setnames(master, "target", paste0("target_", substr(products[i], 4, 20) ))
  rm(target)
  gc()
  
}

targets <- names(master)[grepl("target_",names(master))]

base_target <- master[, mget(c("periodo", targets))]
base_target <- base_target[periodo <= "2018-10-01"]
names(base_target)[-1] <-  sapply(strsplit(targets, "_"), "[[", 2)
base_target <- melt(base_target, id.vars = "periodo")
base_target <- base_target[, .(target = sum(value, na.rm = T), .N ), by = .(periodo, variable)]
base_target[, no_compraron := N - target]
base_target[, compradores := round(target/N, 4)]

# base_target <- base_target[, .(periodo, producto = variable, compradores = target, "No compradores" = no_compraron)]
base_target1 <- base_target[, .(periodo, producto = variable, compradores )]

# 
# base_target <- melt(base_target, id.vars = c("periodo", "producto"))

productos <- sapply(strsplit(targets, "_"), "[[", 2)

plots <- list()
for(i in 1:length(productos) ){
  plot_base <- copy(base_target1[producto == productos[i] ])
  p5 <-  ggplot(plot_base, aes(x = periodo, y = compradores)) +
   geom_bar(stat = "identity", fill = "steelblue") + 
    scale_y_continuous(labels = percent) +
    labs( title = paste("Evolución del porcentaje de compradores a dos meses de", productos[i] ), y = "Porcentaje de clientes" ) +
    theme_minimal()
  ggsave(filename = paste0("Plots/demograficas/evolucion_target_", productos[i],".png"), p5) 
}

##

# staging <-  get.path(staging_path, "201810") %>% readRDS()
staging <- master[periodo == "2018-12-01"]

demograficas <- c("sex",
                  "nivel_educativo",
                  "mar_status",
                  "aa_tipo_vivienda",
                  "aa_estrato",
                  "aa_cod_ciiu",
                  "bb_seg_comercial",
                  "aa_cod_ocupacion",
                  "aa_declara_renta",
                  "age",
                  "antiguedad",
                  "departamento")

demograficas_base <- staging[, mget(demograficas)]
demograficas_base

# sexo 
p6 <- staging[, .N, by = sex][, sex := factor(
  sex,
  levels = c("M", "F", "UNKNOW"),
  labels = c("Masculino", "Fenemino", "Desconocido")
)] %>% ggplot() +
  geom_col(aes(x = sex, y = N, fill = sex)) +
  guides(fill = FALSE) +
  # labs(title = "Tenencia por medio de pago",
  #      subtitle = "") +
  # theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
  geom_text(aes(x = sex, y = N, label = comma(N)),
            size = 2.8,
            vjust = -0.3) +
  scale_y_continuous(labels = comma) +
  labs(title = "Distribución del sexo",
       x = "Sexo",
       y = "Número de clientes") +
  theme_minimal()
ggsave(filename = "Plots/demograficas/dem_sexo.png", p6)

# Nivel educativo 

# p7 <- staging[, .N, by = nivel_educativo][, nivel_educativo := factor(
#   nivel_educativo,
#   levels = c(as.character(0:5), "N", "UNKNOWN"),
#   labels = c(
#     "Ninguno",
#     "Primaria",
#     "Secundaria",
#     "Tecnologo",
#     "Universitaria",
#     "Especializacion",
#     "Otros",
#     "Desconocido"
#   )
# )] 
plot_edu <- staging[, .N, by = nivel_educativo][order(-N)][nivel_educativo == 0, nivel_educativo := "DESCONOCIDO"][, nivel_educativo := factor(
  nivel_educativo,levels = nivel_educativo, ordered = T)]
  
p7 <- plot_edu %>% ggplot(aes(x = nivel_educativo, y = N)) +
  geom_col(aes( fill = nivel_educativo)) +
  guides(fill = FALSE) +
  theme(axis.text.x = element_text(
    angle = -45,
    hjust = 0,
    vjust = 1
  )) +
  geom_text(aes(x = nivel_educativo, y = N, label = comma(N)),
            size = 2.8,
            vjust = -0.3) +
  scale_y_continuous(labels = comma) +
  labs(title = "Distribución del nivel educativo",
       x = "Nivel educativo",
       y = "Número de clientes") +
  theme_minimal()
ggsave(filename = "Plots/demograficas/dem_edu.png", p7)

# marital status
staging[mar_status == "L", mar_status :=  "UNKNOW"]
base_plot <- staging[, .N, by = mar_status][, mar_status := factor(
  mar_status,
  levels = c("C", "D", "O", "P", "S", "U", "V", "UNKNOW"),
  labels = c("CASADO/A",
             "DIVORCIADO/A",
             "OTRO",
             "SEPARADO/A",
             "SOLTERO/A",
             "UNION LIBRE",
             "VIUDO/A",
             "DESCONOCIDO"
             
  )
)][order(-N)]
 
p8 <- base_plot %>% ggplot() +
  geom_col(aes(x = mar_status, y = N, fill = mar_status)) +
  guides(fill = FALSE) +
  scale_x_discrete(limits = base_plot$mar_status) +
  # labs(title = "Tenencia por medio de pago",
  #      subtitle = "") +
   theme(axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1)) +
  geom_text(aes(x = mar_status, y = N, label = comma(N)),
            size = 2.8,
            vjust = -0.3) +
  scale_y_continuous(labels = comma)+
  labs(title = "Distribución del estatus marital",
       x = "Estatus Marital",
       y = "Número de clientes") +
  theme_minimal()
ggsave(filename = "Plots/demograficas/dem_marital.png", p8)

# tipo vivienda 
base_plot <- staging[, .N, by = aa_tipo_vivienda][, aa_tipo_vivienda := factor(
  aa_tipo_vivienda,
  levels = c("F", "P", "UNKNOW", "A", "O"),
  labels = c("Familiar", "Propia", "Desconocido", "Arriendo", "Otros")
)][order(-N)]

p9 <- base_plot%>% ggplot() +
  geom_col(aes(x = aa_tipo_vivienda, y = N, fill = aa_tipo_vivienda)) +
  guides(fill = FALSE) +
  scale_x_discrete(limits = base_plot$aa_tipo_vivienda) +
  # labs(title = "Tenencia por medio de pago",
  #      subtitle = "") +
  # theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
  geom_text(aes(x = aa_tipo_vivienda, y = N, label = comma(N)),
            size = 2.8,
            vjust = -0.3) +
  scale_y_continuous(labels = comma)+
  labs(title = "Distribución del tipo de vivienda",
       x = "Tipo de vivienda",
       y = "Número de clientes") +
  theme_minimal()
ggsave(filename = "Plots/demograficas/dem_vivienda.png", p9)

# estrato 

p10 <- staging[, .N, by = aa_estrato][, aa_estrato := factor(
  aa_estrato,
  levels = c(as.character(0:6)))]%>% ggplot() +
  geom_col(aes(x = aa_estrato, y = N, fill = aa_estrato)) +
  xlab("Estrato") +
  guides(fill = FALSE) +
  ylab("Número de clientes") +
  geom_text(aes(x = aa_estrato, y = N, label = comma(N)),
            size = 2.8,
            vjust = -0.3) +
  scale_y_continuous(labels = comma)+
  labs(title = "Distribución del estrato",
       x = "Estrato",
       y = "Número de clientes") +
  theme_minimal()
ggsave(filename = "Plots/demograficas/dem_estrato.png", p10)

# age
colnames(staging) <- make.unique(names(staging))
p11 <- ggplot(staging, aes(x = age)) +
  geom_histogram(binwidth=.5, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(age, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) +
  labs(title = "Distribución de la edad",
       x = "Edad",
       y = "Número de clientes") + theme_minimal()
ggsave(filename = "Plots/demograficas/dem_edad.png", p11)


# antiguedad 
p12 <- ggplot(staging, aes(x = antiguedad)) +
  geom_histogram(binwidth=.5, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(antiguedad, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) +
  labs(title = "Distribución de la antiguedad en el banco",
       x = "Antiguedad",
       y = "Número de clientes") +
  xlim(c(0, 500)) + theme_minimal()
ggsave(filename = "Plots/demograficas/dem_antig.png", p12)

## financieras

fin <- na.omit(staging[, .(aa_vlr_activos, aa_vlr_ing_bru_mes,
                    aa_vlr_egreso_mes, aa_vlr_pasivos)]) 
names(fin) <- c("Activos", "Ingresos mensuales", "Egresos mensuales", "Pasivos")

fin <- fin %>% GGally::ggpairs(title = "Distrubución, correlación y nube de puntos \n de las variables financieas")
ggsave(filename = "Plots/demograficas/dem_financieras.png", fin)

# departamento

base_plot <- staging[, .N, by = departamento][order(-N)][1:10][, departamento := factor(
  departamento,
  levels = departamento,
  ordered = T
)]

p13 <- base_plot%>% ggplot() +
  geom_col(aes(x = departamento, y = N, fill = departamento)) +
  guides(fill = FALSE) +
  scale_x_discrete(limits = base_plot$departamento) +
  # labs(title = "Tenencia por medio de pago",
  #      subtitle = "") +
  # theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
  geom_text(aes(x = departamento, y = N, label = comma(N)),
            size = 2.8,
            vjust = -0.3) +
  scale_y_continuous(labels = comma)+
  labs(title = "Distribución del los 10 departamentos con más clientes",
       x = "Departamento",
       y = "Número de clientes") +
  theme_minimal()
ggsave(filename = "Plots/demograficas/dem_departamento.png", p13)

# ocupación

base_plot <- demograficas_base[, .N, by = aa_cod_ocupacion][order(-N)][1:5][, aa_cod_ocupacion := factor(
  aa_cod_ocupacion,
  levels = aa_cod_ocupacion,
  ordered = T
)]

p13 <- base_plot%>% ggplot() +
  geom_col(aes(x = aa_cod_ocupacion, y = N, fill = aa_cod_ocupacion)) +
  guides(fill = FALSE) +
  scale_x_discrete(limits = base_plot$aa_cod_ocupacion) +
  # labs(title = "Tenencia por medio de pago",
  #      subtitle = "") +
  # theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
  geom_text(aes(x = aa_cod_ocupacion, y = N, label = comma(N)),
            size = 2.8,
            vjust = -0.3) +
  scale_y_continuous(labels = comma)+
  labs(title = "Distribución del las 5 ocupaciones con más clientes",
       x = "Ocupación",
       y = "Número de clientes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(filename = "Plots/demograficas/dem_ocupacion.png", p13)

## actividad económica 

base_plot <- demograficas_base[, .N, by = aa_cod_ciiu][order(-N)][1:10][, aa_cod_ciiu := factor(
  aa_cod_ciiu,
  levels = aa_cod_ciiu,
  labels = c("Persona natural sin actividad económica",
             "Actividades profesionales",
             "Comercio al por mayor y al por menor",
             "Agricultura, ganadería, etc.",
             "Transporte y almacenamiento",
             "Industrias manufactureras", 
             "Salud y asistencia social",
             "Otras actividades de servicios",
             "Construcción", 
             "Artísticas, entretenimiento y recreación"),
  ordered = T
)]

p14 <- base_plot%>% ggplot() +
  geom_col(aes(x = aa_cod_ciiu, y = N, fill = aa_cod_ciiu)) +
  guides(fill = FALSE) +
  scale_x_discrete(limits = base_plot$aa_cod_ciiu) +
  # labs(title = "Tenencia por medio de pago",
  #      subtitle = "") +
  # theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
  geom_text(aes(x = aa_cod_ciiu, y = N, label = comma(N)),
            size = 2.8,
            vjust = -0.3) +
  scale_y_continuous(labels = comma)+
  labs(title = "Distribución del las 10 actividades económicas con más clientes",
       x = "Actividad económica",
       y = "Número de clientes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(filename = "Plots/demograficas/dem_actividad_eco.png", p14)



