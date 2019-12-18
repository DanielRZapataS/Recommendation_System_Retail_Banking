model_product_path <- "Models/borradores/tesis20190416_tcredito"

load(os.path.join(model_product_path, paste0(products[i], "_objects.RData")))
load(os.path.join(model_product_path, paste0(products[i], "_models.RData")))
model_xgb_auc <- xgb.load(os.path.join(model_product_path, "xgb_auc.model"))
model_xgb_pr <- xgb.load(os.path.join(model_product_path, "xgb_pr.model"))         
        
xgb_auc_preds_master <-  predict(model_xgb_auc, master_dmatrix)
xgb_auc_preds_dev <-  predict(model_xgb_auc, dev_dmatrix)
xgb_auc_preds_test <-  predict(model_xgb_auc, test_dmatrix)

xgb_pr_preds_master <-  predict(model_xgb_pr, master_dmatrix)
xgb_pr_preds_dev <-  predict(model_xgb_pr, dev_dmatrix)
xgb_pr_preds_test <-  predict(model_xgb_pr, test_dmatrix) 

glm_preds_master <- predict(glm, master_h2o) %>% data.table()
glm_preds_dev <- predict(glm, dev_h2o) %>% data.table()
glm_preds_test <- predict(glm, test_h20) %>% data.table()

rf_preds_master <- predict(rf, master_h2o) %>% data.table()
rf_preds_dev <- predict(rf, dev_h2o) %>% data.table()
rf_preds_test <- predict(rf, test_h20)

gbm_preds_master <- predict(gbm, master_h20)
gbm_preds_dev <- predict(gbm, dev_h2o)
gbm_preds_test <- predict(gbm, test_h20)
         
xgb_auc_roc_obj_master <- roc(master$target, xgb_auc_preds_master)
xgb_auc_roc_obj_dev <- roc(dev$target, xgb_auc_preds_dev)
xgb_auc_roc_obj_test <- roc(test$target, xgb_auc_preds_test)

paste("xgb_auc ROC AUC train", pROC::auc(xgb_auc_roc_obj_master))
paste("xgb_auc ROC AUC develop", pROC::auc(xgb_auc_roc_obj_dev))
paste("xgb_auc ROC AUC test", pROC::auc(xgb_auc_roc_obj_test))         

xgb_auc_roc_obj_master <- roc(master$target,glm_preds_master))
xgb_auc_roc_obj_dev <- roc(dev$target, xgb_auc_preds_dev)
xgb_auc_roc_obj_test <- roc(test$target, xgb_auc_preds_test)

paste("xgb_auc ROC AUC train", pROC::auc(xgb_auc_roc_obj_master))
paste("xgb_auc ROC AUC develop", pROC::auc(xgb_auc_roc_obj_dev))
paste("xgb_auc ROC AUC test", pROC::auc(xgb_auc_roc_obj_test))         

         
         
         