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

leer_historial = function(){
  readRDS("Data/Hist_Data.RDS")
}
