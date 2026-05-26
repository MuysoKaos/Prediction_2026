datos = openxlsx::read.xlsx("Data/Hist_Fifa.xlsx", sheet = 1)
valores = openxlsx::read.xlsx("Data/Hist_Fifa.xlsx", sheet = 2)

datos$equipo1[datos$equipo1 == "BAREN"]="BAREIN"
datos$equipo1[datos$equipo1 == "CHECA"]="REPUBLICA CHECA"
datos$equipo1[datos$equipo1 == "ISLAS FEROE"]="ISLAS FAROE"

datos$equipo2[datos$equipo2 == "BAREN"]="BAREIN"
datos$equipo2[datos$equipo2 == "CHECA"]="REPUBLICA CHECA"
datos$equipo2[datos$equipo2 == "ISLAS FEROE"]="ISLAS FAROE"

datos$equipo1 = trimws(toupper(datos$equipo1)) 
datos$equipo2 = trimws(toupper(datos$equipo2))

valores$Equipo = toupper(valores$Equipo)

valores = valores |>
  dplyr::group_by(Equipo) |>
  dplyr::slice(1) |>
  dplyr::ungroup()


datos_mundialistas = datos |>
  dplyr::left_join(valores, by = c("equipo1"="Equipo")) |>
  dplyr::rename(ATK_1 = ATK,
                DEF_1 = DEF)

datos_total = datos_mundialistas|>
  dplyr::left_join(valores, by = c("equipo2"="Equipo")) |>
  dplyr::rename(ATK_2 = ATK,
                DEF_2 = DEF)


head(valores)

saveRDS(datos, file = "Hist_Data.RDS")
saveRDS(valores |>
          dplyr::select(Equipo,ATK,DEF), file = "Value_Data.RDS")
