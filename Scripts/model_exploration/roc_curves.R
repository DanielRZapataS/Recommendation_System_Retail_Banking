#' Functions plots multiple 'roc' objects into one plot
#' @param rocs
#'   A list of 'roc' objects. Every list item has a name.
#' @param breaks
#'   A vector of integers representing ticks on the x- and y-axis
#' @param legentTitel
#'   A string which is used as legend titel
ggrocs <-
  function(rocs,
           breaks = seq(0, 1, 0.1),
           legendTitel = "Modelos",
           producto,
           plotFolder,
           saver
           ) {
    if (length(rocs) == 0) {
      stop("No ROC objects available in param rocs.")
    } else {
      require(plyr)
      # Store all sensitivities and specifivities in a data frame
      # which an be used in ggplot
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
        labs(title = paste("Curvas ROC y AUC para", producto))+
        guides(colour = guide_legend(legendTitel)) +
        theme(axis.ticks = element_line(color = "grey80"))
      png(
        paste0(os.path.join(plotsFolder, saver), ".png"),
        width = 15 * ppi,
        height = 10 * ppi,
        res = ppi
      )
      print(grid.arrange(
        rocPlot,
        matrix,
        nrow = 2,
        as.table = TRUE,
        heights = c(3, 1)
      ))
      
    }
  }

