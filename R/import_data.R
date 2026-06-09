library(ggplot2)
options(scipen = 1111)

historial = readRDS("Data/Hist_Data.RDS")
valores = readRDS("Data/Value_Data.RDS")

datosmodel = crear_data_model(historial,valores) |>
  dplyr::filter(diffatk < 400,
                diffdef < 600)

datosmodel$goles1 = as.numeric(datosmodel$goles1)
datosmodel$goles2 = as.numeric(datosmodel$goles2)

resul = datosmodel |>
  dplyr::filter(equipo1 %in% historial$equipo1) |>
  dplyr::mutate(resul = ifelse(goles1>goles2,"Ganador",
                               ifelse(goles2>goles1,"Losses","Draw")))

avg_w = resul |>
  dplyr::filter(resul == "Ganador") |>
  dplyr::group_by(equipo1) |>
  dplyr::summarise(atK_win = mean(diffatk),
                   deff_win = mean(diffdef))

avg_ls = resul |>
  dplyr::filter(resul == "Losses") |>
  dplyr::group_by(equipo1) |>
  dplyr::summarise(atK_loss = mean(diffatk),
                   deff_loss = mean(diffdef))

datos_w = avg_w|>
  dplyr::left_join(avg_ls, by = "equipo1")|>
  dplyr::mutate(atK_loss = ifelse(is.na(atK_loss) , mean(atK_loss ,na.rm = T),atK_loss),
                deff_loss = ifelse(is.na(deff_loss) , mean(deff_loss ,na.rm = T),deff_loss))

datosmodel = datosmodel |>
  dplyr::left_join(datos_w, by = "equipo1") |>
  dplyr::filter(equipo1 %in% unique(historial$equipo1))

datos_std = datosmodel |>
  dplyr::mutate(diffatk = (diffatk-mean(datosmodel$diffatk))/sd(datosmodel$diffatk),
                diffdef = (diffdef-mean(datosmodel$diffdef))/sd(datosmodel$diffdef),
                atK_win = (atK_win-mean(datosmodel$atK_win))/sd(datosmodel$atK_win),
                deff_win = (deff_win-mean(datosmodel$deff_win))/sd(datosmodel$deff_win),
                atK_loss = (atK_loss-mean(datosmodel$atK_loss))/sd(datosmodel$atK_loss),
                deff_loss = (deff_loss-mean(datosmodel$deff_loss))/sd(datosmodel$deff_loss))

modelo_goles_hechos <- pscl::zeroinfl(goles1 ~   diffatk +atK_win |
                                        diffdef+atK_win,
                                      data = datos_std,
                                      dist = "negbin", link = "probit")

modelo_goles_recibidos <- pscl::zeroinfl(goles2 ~ diffatk + diffdef+atK_loss+deff_loss|
                                           diffdef+atK_loss,
                                         data = datos_std,
                                         dist = "negbin", link = "probit")




#summary(modelo_goles_hechos)
#summary(modelo_goles_recibidos)
