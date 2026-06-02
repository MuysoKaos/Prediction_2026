tab_match = function(id){
  ns = shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(width = 3,
                    shiny::selectInput(ns("equipo1_match"),"EQUIPO 1: ", choices = unique(historial$equipo1),selected = unique(historial$equipo1)[1]),
                    shiny::selectInput(ns("equipo1_goles"),"EQUIPO 1 Goles: ", choices = c(0:10),selected = 0)),
      shiny::column(width = 3,
                    shiny::selectInput(ns("equipo2_match"),"EQUIPO 2: ", choices = unique(c(historial$equipo1,historial$equipo2)),selected = unique(historial$equipo1)[2]),
                    shiny::selectInput(ns("equipo2_goles"),"EQUIPO 2 Goles: ", choices = c(0:10),selected = 0)),
      shiny::column(width = 3,
                    shiny::actionButton(ns("button_match"),"Add Match",class = "btn btn-warning"))
    ),
  )

}
