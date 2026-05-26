###############################################################################
### MODELO DE PREDICCION ##################################

historial = readRDS("Data/Hist_Data.RDS")
valores = readRDS("Data/Value_Data.RDS")


crear_data_model = function(historial,valores){
  
  datos_mundialistas = historial |>
    dplyr::left_join(valores, by = c("equipo1"="Equipo")) |>
    dplyr::rename(ATK_1 = ATK,
                  DEF_1 = DEF)
  
  datos_total = datos_mundialistas|>
    dplyr::left_join(valores, by = c("equipo2"="Equipo")) |>
    dplyr::rename(ATK_2 = ATK,
                  DEF_2 = DEF)
  
  #crear variables
  # modelo:
  # GolesEquipo = ZIP(Equipo | ATK1/DEF2 + DEF1/ATK2 )
  
  datos_total = datos_total |>
    dplyr::mutate(diffatk = ATK_1/DEF_2,
                  diffdef = DEF_1/ATK_2)
  
  datos_rev = datos_total |> 
    dplyr::rename(equipo1 = equipo2,
                  equipo2 = equipo1,
                  goles1 = goles2,
                  goles2 = goles1,
                  ATK_1 = ATK_2,
                  ATK_2 = ATK_1,
                  DEF_2 = DEF_1,
                  DEF_1 = DEF_2) |>
    dplyr::mutate(diffatk = ATK_1/DEF_2,
                  diffdef = DEF_1/ATK_2)
  
  datosmodel = datos_total |>
    dplyr::bind_rows(datos_rev) |>
    dplyr::select(equipo1,equipo2,goles1,goles2,diffatk,diffdef) |>
    unique()
}

datosmodel = crear_data_model(historial,valores)


ggplot2::ggplot(datosmodel, ggplot2::aes(goles1)) + 
  ggplot2::geom_histogram() 

ggplot2::ggplot(datosmodel, ggplot2::aes(goles2)) + 
  ggplot2::geom_histogram() 

#################################################################################
################# MODELO GOLES HECHOS
################################################################################

datosmodel$equipo1 = as.factor(datosmodel$equipo1)

m1 <- pscl ::zeroinfl(goles1 ~  diffatk + diffdef , data = datosmodel, dist = "poisson")

################################################################################
######3 EJEMPLO PREDICCION

# MEXICO 
# SUDaFRICA

datos_equipo1 = valores |>
  dplyr::filter(Equipo == "BRASIL")

datos_equipo2 = valores |>
  dplyr::filter(Equipo == "SUDAFRICA")


input_equipo_1 = data.frame(
  equipo1 = "BRASIL",
  diffatk = datos_equipo1$ATK/datos_equipo2$DEF,
  diffdef = datos_equipo1$DEF/datos_equipo2$ATK
)

input_equipo_2 = data.frame(
  equipo1 = "SUDAFRICA",
  diffatk = datos_equipo2$ATK/datos_equipo1$DEF,
  diffdef = datos_equipo2$DEF/datos_equipo1$ATK
)

predict(m1, input_equipo_1)
predict(m1, input_equipo_2)

probabilidades <- predict(m1, newdata = input_equipo_1, type = "prob")
goles1 <- probabilidades[, c("0", "1", "2", "3", "4", "5")]
goles1 = goles1/sum(goles1)

probabilidades <- predict(m1, newdata = input_equipo_2, type = "prob")
goles2 <- probabilidades[, c("0", "1", "2", "3", "4", "5")]
goles2 = goles2/sum(goles2)

################################################################################
############## MODELO GOLES RECIBIDOS

m2 <- pscl ::zeroinfl(goles2 ~  diffatk + diffdef | equipo1, data = datosmodel)

################################################################################
######3 EJEMPLO PREDICCION

# MEXICO 
# SUDaFRICA

datos_equipo1 = valores |>
  dplyr::filter(Equipo == "MEXICO")

datos_equipo2 = valores |>
  dplyr::filter(Equipo == "SUDAFRICA")


input_equipo_1 = data.frame(
  equipo1 = "MEXICO",
  diffatk = datos_equipo1$ATK/datos_equipo2$DEF,
  diffdef = datos_equipo1$DEF/datos_equipo2$ATK
)

input_equipo_2 = data.frame(
  equipo1 = "SUDAFRICA",
  diffatk = datos_equipo2$ATK/datos_equipo1$DEF,
  diffdef = datos_equipo2$DEF/datos_equipo1$ATK
)

predict(m2, input_equipo_1)
predict(m2, input_equipo_2)

probabilidades <- predict(m2, newdata = input_equipo_1, type = "prob")
goles_rec_1 <- probabilidades[, c("0", "1", "2", "3", "4", "5")]
goles_rec_1 = goles_rec_1/sum(goles_rec_1)

probabilidades <- predict(m2, newdata = input_equipo_2, type = "prob")
goles_rec_2 <- probabilidades[, c("0", "1", "2", "3", "4", "5")]
goles_rec_2 = goles_rec_2/sum(goles_rec_2)

###########################################################################
#pred_final

goles_1_total = (goles1+goles_rec_2)/2
goles_2_total = (goles2+goles_rec_1)/2


############################################################################
###3 SIMULACION

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
  prob = goles_1_total      # Tus probabilidades del modelo ZIP
)

################3 FINAL

simul_tot = data.frame(Goles1 = simulacion_goles1,
           Goles2 = simulacion_goles2)


prob_gan = sum(simul_tot$Goles1>simul_tot$Goles2)/nrow(simul_tot)*100
prob_emp = sum(simul_tot$Goles1==simul_tot$Goles2)/nrow(simul_tot)*100
prob_vis = sum(simul_tot$Goles1<simul_tot$Goles2)/nrow(simul_tot)*100

table(simul_tot$Goles1,simul_tot$Goles2)


library(ggplot2)

# 1. Agrupar los datos para calcular las frecuencias de cada marcador
datos_heatmap <- simul_tot |>
  dplyr::count(Goles1, Goles2) |>
  dplyr::mutate(Porcentaje = (n / sum(n)) * 100) # Porcentaje del total de partidos

library(ggplot2)
# 2. Crear el mapa de calor
ggplot(datos_heatmap, aes(x = factor(Goles1), y = factor(Goles2), fill = Porcentaje)) +
  geom_tile(color = "white", lwd = 0.5, linetype = 1) +
  geom_text(aes(label = sprintf("%.1f%%", Porcentaje)), color = "black", size = 4) +
  scale_fill_gradient(low = "#e0f3db", high = "#43a2ca", name = "% Probabilidad") +
  labs(
    title = "Mapa de Calor de Marcadores Simulados",
    x = "Goles Equipo 1",
    y = "Goles Equipo 2"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


############



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
    x = "Goles Equipo 1",
    y = "Goles Equipo 2"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", vjust = 2),
    axis.text = element_text(face = "bold"),
    # Líneas decorativas para separar visualmente la fila/columna "Total" de la matriz real
    panel.grid = element_blank()
  )
