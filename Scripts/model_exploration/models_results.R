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
library(gridExtra)
library(Metrics)
# library(randomForest)
# h2o.no_progress()
h2o.init()

data_tesis_path <- os.path.join(data_path, "Temporary")

products <<- c("tcredito", "crediservice", "ahorros", "cdt",  
               "libranza", "libredestino", "nomina", "vivienda")


# set random seed for reproducibility
set.seed(5)

cores <- detectCores() - 1 

model_list <- grep("full_models", list.files(models_path), value = T)



for(i in model_list){
  product <- substr(i, 22, 35)
  model_alias_modeling <- os.path.join(models_path, i)
  
  data_id <-  os.path.join(model_alias_modeling, "model_id.csv") %>% fread()
  
  ##### load model data ####
  models_library <- os.path.join(model_alias_modeling, "models_library")
  # h2o.loadModel(models_library)
  # load xgb data 
  load(os.path.join(model_alias_modeling, paste0(product, "_xgb_objects.RData")))
  
  # load xgb precision-recall
  model_xgb_pr <- xgb.load( os.path.join(models_library, "xgb_pr.model"))
  # load xgb roc auc
  model_xgb_auc <- xgb.load( os.path.join(models_library, "xgb_auc.model"))
  
  # load h2o performance 
  load(os.path.join(model_alias_modeling, paste0(product, "_performance.RData")))
  
  # load predictions
  pred_train <- fread(os.path.join(model_alias_modeling, "pred_train.csv"))
  pred_dev <- fread(os.path.join(model_alias_modeling, "pred_dev.csv"))
  

  
  # pred_change <- c("glm_pred", "rf_pred", "gbm_pred")
  # pred_train[, (pred_change) := lapply(.SD, function(x) {
  #   1 - x
  # }), .SDcols = pred_change]
  # pred_dev[, (pred_change) := lapply(.SD, function(x) {
  #   1 - x
  # }), .SDcols = pred_change]
  
  # load importance variable 
  importance_matrix_auc <- fread(os.path.join(
    model_alias_modeling,"xgb_auc_important_variables.csv"
  ))

  importance_matrix_rf <- fread(os.path.join(
    model_alias_modeling,"rf_important_variables.csv"
  ))
  
  


  # load explainers 
  load(os.path.join(model_alias_modeling, paste0(product, "_explainers.RData")))
  
  # load resids 
  load(os.path.join(model_alias_modeling, paste0(product, "_resids.RData")))
  
  # load vip
  load(os.path.join(model_alias_modeling, paste0(product, "_vip.RData")))
  
  # create 
  results_product <- os.path.join(results_path, i)
  dir.create(results_product)
  
  # #### vip random forest ####
  # vip_rf  <-
  #      variable_importance(explainer_rf, n_sample = -1, loss_function = loss_root_mean_square)
  # 
  # ##### selectin variables ####
  # 
  # # selecting variables by permutations
  # num_vars <- 40
  # vip_xgb_auc_table <- data.table(vip_xgb_auc)
  # vip_xgb_auc_table <- vip_xgb_auc_table[order(- dropout_loss)]
  # var_vip <- vip_xgb_auc_table[1:num_vars, variable]
  # var_vip <- var_vip[var_vip %!in% "_baseline_"]
  # 
  # vip_rf_table <- data.table(vip_rf)
  # vip_rf_table <- vip_rf_table[order(- dropout_loss)]
  # var_vip <- vip_xgb_auc_table[1:num_vars, variable]
  # var_vip <- var_vip[var_vip %!in% "_baseline_"]
  # 
  # # selectin variables by gain
  # var_gain <- importance_matrix_auc[order(-Gain)]
  # var_gain <- var_gain[1:num_vars, Feature]
  # 
  # # selectin variables by cover
  # var_cover <- importance_matrix_auc[order(-Cover)]
  # var_cover <- var_cover[1:num_vars, Feature]
  # 
  # # selectin variables by frequency
  # var_freq <- importance_matrix_auc[order(-Frequency)]
  # var_freq <- var_freq[1:num_vars, Feature]
  # 
  # final_variables <-
  #   model_cols[model_cols %in% var_cover |
  #                model_cols %in% var_freq |
  #                model_cols %in% var_gain | model_cols %in% var_vip]
  # saveRDS(object = final_variables, file = os.path.join(results_product, paste0(product, "_final_variables.rds")))
  
  ##### making plots #####
  
#   # residual plot 
#   p1 <- plot(resids_glm, resids_rf, resids_gbm, resids_xgb_auc, resids_xgb_pr)
# ggsave(filename = os.path.join(results_product, paste0(product, "_residual_distribution.png")), plot = p1)
#   
#   
#   p1 <- plot(resids_glm, resids_rf, resids_gbm, resids_xgb_auc, resids_xgb_pr,
#              geom = "boxplot")
#   ggsave(filename = os.path.join(results_product, paste0(product, "_residual_boxplot.png")), plot = p1)
#   
#   # ploting variable importance 
#   
#   # drop-loos permutation
#   p1 <- plot(vip_xgb_auc,max_vars = 20, show_baseline = T)
#   ggsave(filename = os.path.join(results_product, paste0(product, "_vip_xgboost_auc.png")), plot = p1)
#   
#   p1 <- plot(vip_xgb_auc,max_vars = 20)
#   ggsave(filename = os.path.join(results_product, paste0(product, "_vip_xgboost_auc1.png")), plot = p1)
#   
#   
#   # Gain
#   p1 <- xgb.ggplot.importance(importance_matrix_auc, measure = "Gain", rel_to_first = FALSE, top_n = 20)
#   p1 <- p1 + ggplot2::ylab("Gain")
#   ggsave(filename = os.path.join(results_product, paste0(product, "_gain_xgboost_auc.png")), plot = p1)
#   
#   # Cover
#   p1 <- xgb.ggplot.importance(importance_matrix_auc, measure = "Cover", rel_to_first = FALSE, top_n = 20)
#   p1 <- p1 + ggplot2::ylab("Cover")
#   ggsave(filename = os.path.join(results_product, paste0(product, "_cover_xgboost_auc.png")), plot = p1)
#   
#   # Frequency
#   p1 <- xgb.ggplot.importance(importance_matrix_auc, measure = "Frequency", rel_to_first = FALSE, top_n = 20)
#   p1 <- p1 + ggplot2::ylab("Frequency")
#   ggsave(filename = os.path.join(results_product, paste0(product, "_frequency_xgboost_auc.png")), plot = p1)
  
  ##### curvas AUC ####
  glm_roc <- roc(response = pred_dev$target, predictor = pred_dev$glm_pred)
  rf_roc <- roc(response = pred_dev$target, predictor = pred_dev$rf_pred)
  gbm_roc <- roc(response = pred_dev$target, predictor = pred_dev$gbm_pred)
  xgb_auc_roc <- roc(response = pred_dev$target, predictor = pred_dev$xgb_auc_pred)
  xgb_pr_roc <- roc(response = pred_dev$target, predictor = pred_dev$xgb_pr_pred)
  
  rocs <- list("Logit" = glm_roc, "Bosques aleatoreos" = rf_roc,
                   "GBM" = gbm_roc,
                   "XGB-AUC" = xgb_auc_roc,
                   "XGB-PR" = xgb_pr_roc)
  breaks = seq(0, 1, 0.1)
  legendTitel = "Modelos"
  RocVals <- plyr::ldply(names(rocs), function(rocName) {
    if (class(rocs[[rocName]]) != "roc") {
      stop("Please provide roc object from pROC package")
    }
    data.frame(
      fpr = rev(rocs[[rocName]]$specificities),
      tpr = rev(rocs[[rocName]]$sensitivities),
      names = rep(rocName, length(rocs[[rocName]]$sensitivities)),
      stringAsFactors = T
    )
  })
  RocVals <- data.table(RocVals)
  RocVals <- RocVals[sample(.N, 1000)]
  AUC <- sapply(rocs, "[[", "auc")
  aucs <- data.frame(AUC)
  aucs$AUC <- round(aucs$AUC, 2)
  aucs <- t(aucs)
  tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
  matrix <- tableGrob(aucs, theme=tt)
  
  rocPlot <-
    ggplot(RocVals, aes(x = fpr, y = tpr, colour = names)) +
    geom_segment(aes(
      x = 0,
      y = 1,
      xend = 1,
      yend = 0
    ),
    alpha = 0.5,
    colour = "gray") +
    geom_step() +
    scale_x_reverse(name = "Tasa de falsos positivos  (1 - Especificidad)",
                    limits = c(1, 0),
                    breaks = breaks) +
    scale_y_continuous(name = "Tasa de verdadderos positivos (Sensitividad)",
                       limits = c(0, 1),
                       breaks = breaks) +
    theme_bw() +
    coord_equal() +
    labs(title = paste("Curvas ROC y AUC para", product))+
    guides(colour = guide_legend(legendTitel)) +
    theme(axis.ticks = element_line(color = "grey80"))
  # png(
  #   paste0(os.path.join(model_alias_modeling, "auc_all"), ".png"),
  #   width = 15 * ppi,
  #   height = 10 * ppi,
  #   res = ppi
  # )
  
  p1 <- grid.arrange(
    rocPlot,
    matrix,
    nrow = 2,
    as.table = TRUE,
    heights = c(3, 1)
  )
  
  ggsave(file = os.path.join(results_product, paste0(product, "_auc_all.png")),
         plot = p1)
  
  
  
  
  ##### Precison - recall curves ####
  pred_cols <- grep("pred", names(pred_dev), value = T)
  min_max_pred <- min(pred_dev[, lapply(.SD, max), .SDcols = pred_cols])
  
  longitud = 1000
  umbrales <- seq( 0, min_max_pred, length = longitud)
  
  calculate_metrics <- function(target, pred, umbral){
    pred_num <- ifelse(pred >= umbral, 1, 0)
    precision <- Metrics::precision(target, pred_num)
    recall <- Metrics::recall(target, pred_num)
    f1 <- 2*(precision * recall)/(precision + recall) 
    
    out <- t(as.matrix(c(umbral, precision, recall, f1))) 
    return(out)
  }
  rf_pr <- matrix(0, longitud, 4)
  glm_pr <- matrix(0, longitud, 4)
  gbm_pr <- matrix(0, longitud, 4)
  xgb_auc_pr <- matrix(0, longitud, 4)
  xgb_pr_pr <- matrix(0, longitud, 4)
  
  
  for(i in 1:longitud){
    glm_pr[i, ] <- calculate_metrics(pred_dev$target, pred_dev$glm_pred, umbrales[i]) 
    rf_pr[i, ] <- calculate_metrics(pred_dev$target, pred_dev$rf_pred, umbrales[i]) 
    gbm_pr[i, ] <- calculate_metrics(pred_dev$target, pred_dev$gbm_pred, umbrales[i]) 
    xgb_auc_pr[i, ] <- calculate_metrics(pred_dev$target, pred_dev$xgb_auc_pred, umbrales[i]) 
    xgb_auc_pr[i, ] <- calculate_metrics(pred_dev$target, pred_dev$xgb_pr_pred, umbrales[i]) 
  }
  ppi = 300
  png(
    os.path.join(results_product, paste0(product, "_Precisión.png")),
    width = 7 * ppi,
    height = 7 * ppi,
    res = ppi
  )
  plot(rf_pr[,1], rf_pr[,2],xlab="Umbrales",ylab="Precisión",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),
       type="l",lwd=2,axes=FALSE,col=2)
  axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  lines(rf_pr[,1],glm_pr[,2],col=3,lwd=2)
  lines(rf_pr[,1],gbm_pr[,2],col=4,lwd=2)
  lines(rf_pr[,1], xgb_auc_pr[,2],col=5,lwd=2)
  lines(rf_pr[,1], xgb_pr_pr[,2],col=6,lwd=2)
  box()
  legend("bottomright",col=c(2:6),text.font = 1,inset = 0.01,
         box.lty=0,cex = 1, 
         lwd=c(2,2,2,2),c("RF", "glm", "gbm", "xgb_auc", "xgb_pr"))
  axis(1, at = seq(0.1, 1, by = 0.1))
  dev.off()
  
  png(
    os.path.join(results_product, paste0(product, "_exhaustividad.png")),
    width = 7 * ppi,
    height = 7 * ppi,
    res = ppi
  )
  plot(rf_pr[,1], rf_pr[,3],xlab="Umbrales",ylab="Exhaustividad",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),
       type="l",lwd=2,axes=FALSE,col=2)
  axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  lines(rf_pr[,1],glm_pr[,3],col=3,lwd=2)
  lines(rf_pr[,1],gbm_pr[,3],col=4,lwd=2)
  lines(rf_pr[,1], xgb_auc_pr[,3],col=5,lwd=2)
  lines(rf_pr[,1], xgb_pr_pr[,3],col=6,lwd=2)
  box()
  legend("bottomright",col=c(2:6),text.font = 1,inset = 0.01,
         box.lty=0,cex = 1, 
         lwd=c(2,2,2,2),c("RF", "glm", "gbm", "xgb_auc", "xgb_pr"))
  axis(1, at = seq(0.1, 1, by = 0.1))
  dev.off()
  
  png(
    os.path.join(results_product, paste0(product, "_f1.png")),
    width = 7 * ppi,
    height = 7 * ppi,
    res = ppi
  )
  plot(rf_pr[,1], rf_pr[,4],xlab="Umbrales",ylab="F1",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),
       type="l",lwd=2,axes=FALSE,col=2)
  axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  lines(rf_pr[,1],glm_pr[,4],col=3,lwd=2)
  lines(rf_pr[,1],gbm_pr[,4],col=4,lwd=2)
  lines(rf_pr[,1], xgb_auc_pr[,4],col=5,lwd=2)
  lines(rf_pr[,1], xgb_pr_pr[,4],col=6,lwd=2)
  box()
  legend("bottomright",col=c(2:6),text.font = 1,inset = 0.01,
         box.lty=0,cex = 1, 
         lwd=c(2,2,2,2),c("RF", "glm", "gbm", "xgb_auc", "xgb_pr"))
  axis(1, at = seq(0.1, 1, by = 0.1))
  dev.off()
  
  png(
    os.path.join(results_product, paste0(product, "_presicion_recall.png")),
    width = 7 * ppi,
    height = 7 * ppi,
    res = ppi
  )
    plot(rf_pr[,2], rf_pr[,3],xlab="Precisión",ylab="Exhaustividad",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),
       type="l",lwd=2,axes=FALSE,col=2)
  axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  lines(rf_pr[,2],glm_pr[,3],col=3,lwd=2)
  lines(rf_pr[,2],gbm_pr[,3],col=4,lwd=2)
  lines(rf_pr[,2], xgb_auc_pr[,3],col=5,lwd=2)
  lines(rf_pr[,2], xgb_pr_pr[,3],col=6,lwd=2)
  box()
  legend("bottomright",col=c(2:6),text.font = 1,inset = 0.01,
         box.lty=0,cex = 1, 
         lwd=c(2,2,2,2),c("RF", "glm", "gbm", "xgb_auc", "xgb_pr"))
  axis(1, at = seq(0.1, 1, by = 0.1))
  dev.off()
  
    
}







