library(ggplot2)

historial = readRDS("Data/Hist_Data.RDS")
valores = readRDS("Data/Value_Data.RDS")

datosmodel = crear_data_model(historial,valores)

modelo_goles_hechos <- pscl ::zeroinfl(goles1 ~   diffatk + diffdef , data = datosmodel,
                                       family = "poisson", link = "logit")

modelo_goles_recibidos <- pscl ::zeroinfl(goles2 ~ diffatk + diffdef, data = datosmodel,
                                          family = "poisson", link = "logit")
