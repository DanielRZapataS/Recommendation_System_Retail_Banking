#' Title
#'
#' @param models_path : where product models are allocated
#' @param models : models alias to include in the next product to buy
#' @param date_to_predict : month to make prediction
#' @param performance : if month to predit have a historical target TRUE els FALSE
#' @param results_path : where results are allocated
#' @param model_alias_npb : models alias to ensample product models
#'
#' @return : return on results pth the next product model
create_npb <- function(models_path,
                       models,
                       date_to_predict,
                       performance_npb,
                       results_path,
                       model_alias_npb) {
  
  models_files <- list.files(models_path)
  models_files <- models_files[models_files %in% models]
  products <- sapply(strsplit(models_files, "_"), "[[", 2)
  
  if(sum(duplicated(products))){stop("There is duplicated models products")}
  
  models_files_table <- data.table(models = models_files, products = products) 
  
  print(paste("Making Next Product to buy", date_to_predict))
  ifelse(performance_npb,
         results_path <- os.path.join(results_path, "Performance"),
         results_path <- os.path.join(results_path, "Prediction"))
  
  dir.create(os.path.join(results_path, date_to_predict))
  results_alias_path <-
    os.path.join(results_path, date_to_predict ,model_alias_npb)
  dir.create(results_alias_path)
  
  print("Upload master table")
  master <- get.path(master_path, "master") %>% readRDS()
  
  test_cut <-
    as.Date(paste0(as.character(date_to_predict), '01'), format = '%Y%m%d')
  months_cut <- c(test_cut, floor_date(as.Date(test_cut) + months(2), "month"))
  master <- master[periodo %in% months_cut] 
  
  if(performance_npb){
    for(i in 1:nrow(models_files_table)){
      print(paste0("Creating target variable ", products[i]))
      var_target <- paste0("pr_", products[i])
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
      setnames(master, "target", paste0("target_", products[i]))
      rm(target)
      gc()
      
    }
  }
  
  # add in purchase frequency feature for each product
  print("Load purchase frequencies")
  
  purchase.frequencies <-
    readRDS(get.path(feature_path,  get_month(1)))
  
  purchase.frequencies <- data.table(purchase.frequencies)
  
  master   <-
    merge(master,
          purchase.frequencies,
          by = c("month.id", "llave"),
          all.x = TRUE)
  master[is.na(master)] <- 0
  rm(purchase.frequencies)
  gc()
  
  # create train and test tables
  print("Creating score tables")
  # divinding master table
  test_full <- master[periodo == test_cut]
  test_full[is.na(test_full)] <- 0
  rm(master)
  gc()
  
  
  # load models and make predictions
  models <- models_files_table$models
  for(i in 1:nrow(models_files_table)){
    test <- copy(test_full)
    
    all_variables <- names(test)
    lags <- names(test)[grepl("month_ago", names(test))]
    lags_product <- lags[grepl(products[i], lags)]
    last_owned <- names(test)[grepl("last.owned", names(test))]
    last_owned_product <- last_owned[grepl(products[i], last_owned)]
    purchase_count <- names(test)[grepl("purchase.count", names(test))]
    purchase_count_product <- purchase_count[grepl(products[i], purchase_count)]
    target <- names(test)[grepl("target", names(test))]
    selected_var <-
      all_variables[all_variables %!in% c(lags, last_owned, purchase_count, target)]
    selected_var <-
      c(selected_var,
        lags_product,
        last_owned_product,
        purchase_count_product)
    test <- test[,  mget(selected_var)]
    
    # Classifing variables into categories
    id_variables <-
      c("llave", "periodo", "month.id", "month", "year", paste0("target_", products))
    products_variables <- names(test)[grepl("pr_", names(test))]
    products_variables <-
      c(products_variables, "total_products", "num.transactions")
    crm_vars <-
      names(test)[names(test) %!in% c(id_variables, products_variables)]
    
    categorical_cols <-
      c(crm_vars[sapply(test[, mget(crm_vars)], is.factor)],
        "month", "year")
    categorical_cols <-
      categorical_cols[categorical_cols %!in% c("bb_seg_comercial", "aa_cod_ocupacion")]
    
    numeric_cols <-
      c(crm_vars[!(sapply(test[, mget(crm_vars)], is.factor))],
        products_variables,
        c("bb_seg_comercial", "aa_cod_ocupacion"))
    
    # one-hot encode the categorical features
    ohe_test <- dummyVars( ~ ., data = test[, mget(categorical_cols)])
    ohe_test <- predict(ohe_test, test[, mget(categorical_cols)])
    ohe_cols <- colnames(ohe_test)
    ohe_test <- as(data.matrix(ohe_test), "dgCMatrix")
    
    # data to train and predict
    test_dmatrix       <-
      cbind(ohe_test, data.matrix(test[, mget(numeric_cols)]))
    rm( ohe_test)
    gc()
    
    print(paste0("load model ", models[i]))
    model_alias_path <-
      os.path.join(models_path, models[i])
    model <-  xgb.load(os.path.join(model_alias_path, paste0(models[i], ".model")))
    print(paste0("predicting ", products[i], " based on model ", models[i]))
    test_full[, pred := predict(model, test_dmatrix)]
    setnames(test_full, "pred", paste0("pred_", products[i]))
    
  }
  test <- test_full
  rm(test_full)
  gc()
  
  if(performance_npb){
    fwrite(test[, mget(c("llave", "periodo", paste0("target_", products), 
                         paste0("pred_", products)))],
           os.path.join(results_alias_path, "pred_npb.csv"))
    
    targets <- names(test)[grepl("target_", names(test))]
    products <- names(test)[str_detect(names(test), "target")]
    products <- sapply(strsplit(products, "_"), "[[", 2)
    
    ################################################### 
    
    ### Perfect MAP@K ### 
    # recomend every first product each client bought
    test[, count := rowSums(test[, mget(targets)])]
    fwrite(test[, .N, by = count][order(-N)],
           os.path.join(results_alias_path, "count_npb.csv"))

    #####################
    ###### METRICS ######
    #####################
    
    #####################
    ####### MAP@K #######
    #####################
    
    # product adquicision matrix (only clients who agregate products)
    
    cols <- c("llave", targets)
    products_acq <- test[, mget(cols)]
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
    probs <- names(test)[grepl("pred_" , names(test))]
    probs <- probs[probs %in% paste0("pred_", products)]
    cols <- c("llave", probs)
    products_pred <- test[, mget(cols)]
    names(products_pred)[-1] <- products
    products_pred  <- melt(products_pred , id.vars = c("llave"))
    products_pred[, pred_order := rank(-value), by = .(llave) ]
    setkey(products_pred, "llave", "pred_order")
    # products_pred[llave == "631322"]
    
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
    
    ## MAPK
    
    actual <- c(products_acq_list, products_acq_list_loss)
    predicted <- c(products_pred_list, products_pred_list_loss)
    
    length(actual)
    length(actual) == length(predicted)
    
    mapk <- c()
    for(i in 1:9){
      print(mapk(i, actual, predicted))
      mapk[i] <-  mapk(i, actual, predicted)
    }
    mapk <- data.table(k = 1:9, mapk = mapk)
    fwrite(mapk, os.path.join(results_alias_path, "mapk.csv"))
    
    # # change matrix results 
    # 
    # response <- melt(test[, mget(paste0("target_", products))])
    # predictor <- melt(test[, mget(paste0("pred_num_", products))])
    # 
    # ##############################
    # ####### ACCURACY #######
    # ##############################
    # # the proportion of elements in actual that are equal to the corresponding
    # # element in predicted
    # accuracy(response$value, predictor$value)
    # 
    # ########################
    # ####### PRECISON #######
    # ########################
    # # precision computes proportion of observations predicted to be in the positive
    # # class (i.e. the element in predicted equals 1) that actually belong to the
    # # positive class (i.e. the element in actual equals 1)
    # Metrics::precision(response$value, predictor$value)
    # 
    # ######################
    # ####### RECALL #######
    # ######################
    # # recall computes proportion of observations in the positive class (i.e. the
    # # element in actual equals 1) that are predicted to be in the positive class
    # # (i.e. the element in predicted equals 1)
    # Metrics::recall(response$value, predictor$value)
    # 
    
  }else{
    fwrite(test[, mget(c("llave", "periodo", paste0("pred_", products)))],
           os.path.join(results_alias_path, "pred_npb.csv"))
    
    products <- names(test)[grepl("pred_", names(test))]
    products <- sapply(strsplit(products, "_"), "[[", 2)
    
    # product propencity matrix
    probs <- names(test)[grepl("pred_", names(test))]
    cols <- c("llave", probs)
    products_pred <- test[, mget(cols)]
    names(products_pred)[-1] <- products
    products_pred  <- melt(products_pred , id.vars = c("llave"))
    products_pred[, pred_order := rank(-value), by = .(llave) ]
    fwrite(products_pred, os.path.join(results_alias_path, "pred_npb_long.csv"))
  }
  
}



