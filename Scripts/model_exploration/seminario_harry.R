
####Gráfico ICE


p1_ice <- partial(modelo, pred.var = "dias_ult_tran_mon", ice = TRUE, center = TRUE, 
                  plot = TRUE, rug = TRUE, alpha = 0.1, plot.engine = "ggplot2", 
                  train = data.matrix(Train))


###Gráfico de dependencia parcial pdp

p1 <- modelo %>%  # the %>% operator is read as "and then"
  partial(pred.var = "dias_ult_tran_mon", train = data.matrix(Train)) %>%
  plotPartial(smooth = TRUE, lwd = 2, ylab = expression(f(prob)),
              main = "días desde última transacción monetaria-based PDP")



###Mapa de calor
p3_pdp <- partial(modelo, pred.var = c("dias_ult_tran_mon", "Antiguedad"),
                  plot = TRUE, chull = TRUE, plot.engine = "ggplot2", train = data.matrix(Train))





####Gráfico en 3d

pd <- partial(modelo, pred.var = c("dias_ult_tran_mon", "Antiguedad"), train = data.matrix(Train))
p4_pdp <- plotPartial(pd, levelplot = FALSE, zlab = "g(prob)", colorkey = TRUE, 
                      screen = list(z = -20, x = -60))



