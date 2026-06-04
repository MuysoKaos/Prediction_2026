library(ggplot2)
options(scipen = 1111)

historial = readRDS("Data/Hist_Data.RDS")
valores = readRDS("Data/Value_Data.RDS")

datosmodel = crear_data_model(historial,valores)
datosmodel$goles1 = as.numeric(datosmodel$goles1)
datosmodel$goles2 = as.numeric(datosmodel$goles2)

modelo_goles_hechos <- pscl ::zeroinfl(goles1 ~   diffatk + diffdef  , data = datosmodel,
                                       family = "poisson", link = "logit")

modelo_goles_recibidos <- pscl ::zeroinfl(goles2 ~ diffatk + diffdef, data = datosmodel,
                                          family = "poisson", link = "logit")


#summary(modelo_goles_hechos)
#summary(modelo_goles_recibidos)
