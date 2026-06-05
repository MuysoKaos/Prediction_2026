historial = readRDS("Data/Hist_Data.RDS")
valores = readRDS("Data/Value_Data.RDS")

datosmodel = crear_data_model(historial,valores)
datosmodel$goles1 = as.numeric(datosmodel$goles1)
datosmodel$goles2 = as.numeric(datosmodel$goles2)


dist_opt = c("poisson", "negbin", "geometric")
link_opt = c("logit", "probit", "cloglog", "cauchit", "log")


evaluar = historial |>
  dplyr::filter(equipo2 %in% unique(historial$equipo1))

i = 1
dst = dist_opt[1]
lnk = link_opt[1]
data_comparisson = data.frame()
for(dst in dist_opt){
  for(lnk in link_opt){

    act = paste0(dst," ",lnk)
    print(act)

    if(!act %in% c("poisson log","negbin log","geometric log")){
      modelo_goles_hechos <- pscl ::zeroinfl(goles1 ~   -1+diffatk +diffdef    , data = datosmodel,
                                             dist  = dst, link  = lnk)

      modelo_goles_recibidos <- pscl ::zeroinfl(goles2 ~  -1+diffatk +diffdef , data = datosmodel,
                                                dist = dst, link = lnk)

      ############################################################################
      ##### EVALUAR PARTIDOS
      ############################################################################

      matriz_eval = data.frame(evaluar) |>
        dplyr::mutate(pred_goles1 = NA,
                      pred_goles2 = NA)

      for(i in 1:nrow(evaluar)){

        ######## SIMULACION EQUIPO 1

        datos_equipo1 = valores |>
          dplyr::filter(Equipo == evaluar$equipo1[i])

        datos_equipo2 = valores |>
          dplyr::filter(Equipo == evaluar$equipo2[i])


        input_equipo_1 = data.frame(
          equipo1 = evaluar$equipo1[i],
          diffatk = datos_equipo1$ATK/datos_equipo2$DEF,
          diffdef = datos_equipo1$DEF/datos_equipo2$ATK
        )

        probabilidades <- predict(modelo_goles_hechos, newdata = input_equipo_1, type = "prob")
        goles1 <- probabilidades[, c("0", "1", "2", "3", "4", "5")]
        goles1 = goles1/sum(goles1)

        probabilidades <- predict(modelo_goles_recibidos, newdata = input_equipo_1, type = "prob")
        goles_rec_1 <- probabilidades[, c("0", "1", "2", "3", "4", "5")]
        goles_rec_1 = goles_rec_1/sum(goles_rec_1)

        ######################## DATOS EQUIPO 2

        input_equipo = data.frame(
          equipo1 =  evaluar$equipo2[i],
          diffatk = datos_equipo2$ATK/datos_equipo1$DEF,
          diffdef = datos_equipo2$DEF/datos_equipo1$ATK
        )

        probabilidades <- predict(modelo_goles_hechos, newdata = input_equipo, type = "prob")
        goles2 <- probabilidades[, c("0", "1", "2", "3", "4", "5")]
        goles2 = goles2/sum(goles2)

        probabilidades <- predict(modelo_goles_recibidos, newdata = input_equipo, type = "prob")
        goles_rec_2 <- probabilidades[, c("0", "1", "2", "3", "4", "5")]
        goles_rec_2 = goles_rec_2/sum(goles_rec_2)

        #################################################################

        ###3 SIMULACION
        goles_1_total = (goles1+goles_rec_2)/2
        goles_2_total = (goles2+goles_rec_1)/2

        pred_1 = goles1_pred = names(goles_1_total)[which.max(goles_1_total)]
        pred_2 = goles2_pred = names(goles_2_total)[which.max(goles_2_total)]

        matriz_eval$pred_goles1[i] = pred_1
        matriz_eval$pred_goles2[i] = pred_2
      }

      #### evaluacion del modelo final

      matriz_eval = matriz_eval |>
        dplyr::mutate(real =  ifelse(goles1>goles2,"Equipo1",
                                     ifelse(goles2>goles1, "Equipo2",
                                            "Empate")),
                      pred =ifelse(pred_goles1 >pred_goles2,"Equipo1",
                                   ifelse(pred_goles2>pred_goles1 , "Equipo2",
                                          "Empate")))

      ##### FInal

      resul = data.frame(Dist = dst,
                         Link = lnk,
                         pred_goles1 = sum(matriz_eval$goles1 == matriz_eval$pred_goles1)/nrow(matriz_eval),
                         pred_goles2 = sum(matriz_eval$goles2 == matriz_eval$pred_goles2)/nrow(matriz_eval),
                         pred_resul = sum(matriz_eval$real == matriz_eval$pred)/nrow(matriz_eval))

      data_comparisson = rbind(data_comparisson,resul)
    }




  }
}

# modelo 1 sin equipo poisson logit 0.5260870
# modelo 1 con equipo negbin probit 0.5043478
# modelo 2 sin equipo poisson logit 0.5217391
# modelo3 = modelo 1 sin intercept
