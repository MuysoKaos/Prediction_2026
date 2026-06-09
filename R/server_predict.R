server_predict = function(id){
  shiny::moduleServer(id,function(input,output,session){

    ns = session$ns

    ######################################################
    ##### REACTIVE
    ######################################################

    equipo1_ = shiny::reactive({input$equipo1})
    equipo2_ = shiny::reactive({input$equipo2})

    datos_equipo1_ = shiny::reactive({
      equipo = equipo1_()

      valores |>
        dplyr::filter(Equipo == equipo)|>
        dplyr::left_join(datos_w, by = c("Equipo"="equipo1"))
    })

    datos_equipo2_ = shiny::reactive({
      equipo = equipo2_()

      valores |>
        dplyr::filter(Equipo == equipo)|>
        dplyr::left_join(datos_w, by = c("Equipo"="equipo1"))
    })

    data_team_1_ = shiny::reactive({

      equipo = equipo1_()
      datos_equipo1 = datos_equipo1_()
      datos_equipo2 = datos_equipo2_()

      #save(equipo,datos_equipo1,datos_equipo2, file = "tosee_data_team_1.RData")

      input_equipo_1 = data.frame(
        equipo1 = equipo,
        diffatk = datos_equipo1$ATK/datos_equipo2$DEF,
        diffdef = datos_equipo1$DEF/datos_equipo2$ATK,
        atK_win = datos_equipo1$atK_win,
        deff_win = datos_equipo1$deff_win,
        atK_loss = datos_equipo1$atK_loss,
        deff_loss = datos_equipo1$deff_loss
      )

      input_equipo_1_norm = input_equipo_1 |>
        dplyr::mutate(diffatk = (diffatk-mean(datosmodel$diffatk))/sd(datosmodel$diffatk),
                      diffdef = (diffdef-mean(datosmodel$diffdef))/sd(datosmodel$diffdef),
                      atK_win = (atK_win-mean(datosmodel$atK_win))/sd(datosmodel$atK_win),
                      deff_win = (deff_win-mean(datosmodel$deff_win))/sd(datosmodel$deff_win),
                      atK_loss = (atK_loss-mean(datosmodel$atK_loss))/sd(datosmodel$atK_loss),
                      deff_loss = (deff_loss-mean(datosmodel$deff_loss))/sd(datosmodel$deff_loss))


      probabilidades <- predict(modelo_goles_hechos, newdata = input_equipo_1_norm, type = "prob")

      goles1 <- probabilidades[, c("0", "1", "2", "3", "4", "5")]
      goles1 = goles1/sum(goles1)

      probabilidades <- predict(modelo_goles_recibidos, newdata = input_equipo_1_norm, type = "prob")


      goles_rec_1 <- probabilidades[, c("0", "1", "2", "3", "4", "5")]
      goles_rec_1 = goles_rec_1/sum(goles_rec_1)

      return(list(goles1 =goles1,
                  goles_rec_1 = goles_rec_1))

    })

    data_team_2_ = shiny::reactive({

      equipo = equipo2_()
      datos_equipo1 = datos_equipo2_()
      datos_equipo2 = datos_equipo1_()

      #save(equipo,datos_equipo1,datos_equipo2, file = "tosee_data_team_2.RData")


      input_equipo = data.frame(
        equipo1 = equipo,
        diffatk = datos_equipo1$ATK/datos_equipo2$DEF,
        diffdef = datos_equipo1$DEF/datos_equipo2$ATK,
        atK_win = datos_equipo1$atK_win,
        deff_win = datos_equipo1$deff_win,
        atK_loss = datos_equipo1$atK_loss,
        deff_loss = datos_equipo1$deff_loss
      )

      input_equipo_norm = input_equipo |>
        dplyr::mutate(diffatk = (diffatk-mean(datosmodel$diffatk))/sd(datosmodel$diffatk),
                      diffdef = (diffdef-mean(datosmodel$diffdef))/sd(datosmodel$diffdef),
                      atK_win = (atK_win-mean(datosmodel$atK_win))/sd(datosmodel$atK_win),
                      deff_win = (deff_win-mean(datosmodel$deff_win))/sd(datosmodel$deff_win),
                      atK_loss = (atK_loss-mean(datosmodel$atK_loss))/sd(datosmodel$atK_loss),
                      deff_loss = (deff_loss-mean(datosmodel$deff_loss))/sd(datosmodel$deff_loss))

      probabilidades <- predict(modelo_goles_hechos, newdata = input_equipo_norm, type = "prob")

      goles1 <- probabilidades[, c("0", "1", "2", "3", "4", "5")]
      goles1 = goles1/sum(goles1)

      probabilidades <- predict(modelo_goles_recibidos, newdata = input_equipo_norm, type = "prob")

      goles_rec_1 <- probabilidades[, c("0", "1", "2", "3", "4", "5")]
      goles_rec_1 = goles_rec_1/sum(goles_rec_1)

      return(list(goles2 =goles1,
                  goles_rec_2 = goles_rec_1))

    })

    simulador_ = shiny::reactive({

      data_team_1 = data_team_1_()
      data_team_2 = data_team_2_()

      #save(data_team_1,data_team_2, file = "tosee_simulador.RData")

      ###3 SIMULACION
      goles_1_total = (data_team_1$goles1+data_team_2$goles_rec_2)/2
      goles_2_total = (data_team_2$goles2+data_team_1$goles_rec_1)/2

      simulacion_goles1 <- sample(
        x = 0:5,                  # Los valores posibles a simular
        size = 10000,              # Cantidad de simulaciones
        replace = TRUE,           # Reemplazo TRUE porque podemos repetir los mismos goles
        prob = goles_1_total      # Tus probabilidades del modelo ZIP
      )

      simulacion_goles2 <- sample(
        x = 0:5,                  # Los valores posibles a simular
        size = 10000,              # Cantidad de simulaciones
        replace = TRUE,           # Reemplazo TRUE porque podemos repetir los mismos goles
        prob = goles_2_total      # Tus probabilidades del modelo ZIP
      )

      ################3 FINAL

      simul_tot = data.frame(Goles1 = simulacion_goles1,
                             Goles2 = simulacion_goles2)

      simul_tot
    })

    #########################################################
    ######3 OUTPUT
    #########################################################

    output$prob_equipo1 = shiny::renderText({
      simul_tot = simulador_()

      x = round(sum(simul_tot$Goles1>simul_tot$Goles2)/nrow(simul_tot)*100,2)

      paste0(x,"%")
    })

    output$prob_empate = shiny::renderText({
      simul_tot = simulador_()

      x = round(sum(simul_tot$Goles1==simul_tot$Goles2)/nrow(simul_tot)*100,2)

      paste0(x,"%")
    })

    output$prob_equipo2 = shiny::renderText({
      simul_tot = simulador_()

      x = round(sum(simul_tot$Goles1<simul_tot$Goles2)/nrow(simul_tot)*100,2)

      paste0(x,"%")
    })

    output$plot_heatmap = shiny::renderPlot({
      simul_tot = simulador_()
      equipo1 = equipo1_()
      equipo2 = equipo2_()

      # 1. Calcular la matriz base en porcentajes
      base_marcardores <- simul_tot |>
        dplyr::count(Goles1, Goles2) |>
        dplyr::mutate(Porcentaje = (n / sum(n)) * 100) |>
        dplyr::select(-n)

      # 2. Convertir a formato texto para poder incluir la categoría "Total"
      datos_completos <- base_marcardores |>
        dplyr::mutate(
          Goles1 = as.character(Goles1),
          Goles2 = as.character(Goles2)
        )

      # 3. Calcular los totales marginales (Fila Total y Columna Total)
      total_goles1 <- datos_completos |>
        dplyr::group_by(Goles1) |>
        dplyr::summarise(Porcentaje = sum(Porcentaje), .groups = "drop") |>
        dplyr::mutate(Goles2 = "Total")

      total_goles2 <- datos_completos |>
        dplyr::group_by(Goles2)|>
        dplyr::summarise(Porcentaje = sum(Porcentaje), .groups = "drop") |>
        dplyr::mutate(Goles1 = "Total")

      # Total general (esquina inferior derecha, sumará 100%)
      total_general <- data.frame(Goles1 = "Total", Goles2 = "Total", Porcentaje = 100)

      # 4. Unir todo en un solo dataframe
      datos_heatmap <- dplyr::bind_rows(datos_completos, total_goles1, total_goles2, total_general)

      # 5. Definir el orden de los factores para que "Total" quede al final/abajo
      orden_x <- c(as.character(0:5), "Total")
      orden_y <- c("Total", as.character(5:0)) # Invertido para que 0 quede abajo y Total hasta abajo

      datos_heatmap <- datos_heatmap |>
        dplyr::mutate(
          Goles1 = factor(Goles1, levels = orden_x),
          Goles2 = factor(Goles2, levels = orden_y)
        )

      # 6. Graficar con el eje X en la parte superior
      ggplot(datos_heatmap, aes(x = Goles1, y = Goles2, fill = Porcentaje)) +
        geom_tile(color = "white", lwd = 0.5) +
        geom_text(aes(label = sprintf("%.1f%%", Porcentaje)), color = "black", size = 3.5) +
        # Usamos una escala de color que resalte los totales si deseas, o una continua clásica:
        scale_fill_gradient(low = "#f7fbff", high = "#08306b", name = "% Prob") +
        # Mover el eje X a la parte superior
        scale_x_discrete(position = "top") +
        labs(
          title = "Mapa de Calor de Marcadores Simulados con Totales",
          x = paste0("Goles ",equipo1),
          y = paste0("Goles ",equipo2)
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", vjust = 2),
          axis.text = element_text(face = "bold"),
          # Líneas decorativas para separar visualmente la fila/columna "Total" de la matriz real
          panel.grid = element_blank()
        )

    })

  })
}
