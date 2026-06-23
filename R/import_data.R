library(ggplot2)
options(scipen = 1111)

historial = readRDS("Data/Hist_Data.RDS")
valores = readRDS("Data/Value_Data.RDS")

datosmodel = crear_data_model(historial,valores)

datosmodel$goles1 = as.numeric(datosmodel$goles1)
datosmodel$goles2 = as.numeric(datosmodel$goles2)

resul = datosmodel |>
  dplyr::filter(equipo1 %in% historial$equipo1) |>
  dplyr::mutate(resul = ifelse(goles1>goles2,"Ganador",
                               ifelse(goles2>goles1,"Losses","Draw"))) |>
  dplyr::left_join(valores, by = c("equipo2"="Equipo")) |>
  dplyr::group_by(equipo1) |>
  dplyr::mutate(encuentros = dplyr::n()) |>
  dplyr::ungroup()


avg_w = resul |>
  dplyr::filter(resul != "Losses") |>
  dplyr::group_by(equipo1) |>
  dplyr::summarise(atK_win = mean(ATK)*dplyr::n()/mean(encuentros),
                   deff_win = mean(DEF)*dplyr::n()/mean(encuentros))

avg_ls = resul |>
  dplyr::filter(resul == "Losses") |>
  dplyr::group_by(equipo1) |>
  dplyr::summarise(atK_loss = mean(ATK)*dplyr::n()/mean(encuentros),
                   deff_loss = mean(DEF)*dplyr::n()/mean(encuentros))

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

datos_std_out = datos_std |>
  dplyr::filter(diffatk <= 3,
                diffdef <= 3,
                atK_win<= 3,
                deff_win<= 3,
                atK_loss <= 3,
                deff_loss <= 3)

#diffatk+diffdef+atK_win+deff_win+atK_loss+deff_loss
modelo_goles_hechos <- pscl::zeroinfl(goles1 ~   diffatk+atK_win |
                                        diffdef+deff_win,
                                      data = datos_std_out,
                                      dist = "negbin", link = "probit")

#summary(modelo_goles_hechos)

modelo_goles_recibidos <- pscl::zeroinfl(goles2 ~ diffatk+diffdef+deff_win |
                                           deff_win,
                                         data = datos_std_out,
                                         dist = "negbin", link = "probit")


#summary(modelo_goles_recibidos)
