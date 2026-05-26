tab_predict = function(id){
  ns = shiny::NS(id)
  
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(width = 3,
                    shiny::selectInput(ns("equipo1"),"EQUIPO 1: ", choices = unique(historial$equipo1),selected = unique(historial$equipo1)[1]),
                    shiny::selectInput(ns("equipo2"),"EQUIPO 2: ", choices = unique(historial$equipo1),selected = unique(historial$equipo1)[2])),
      shiny::column(width = 3,
                    bslib::value_box(
                      title = "EQUIPO 1",
                      value = shiny::textOutput(ns("prob_equipo1"))
                    ),
                    bslib::value_box(
                      title = "EMPATE",
                      value = shiny::textOutput(ns("prob_empate"))
                    ),
                    bslib::value_box(
                      title = "EQUIPO 2",
                      value = shiny::textOutput(ns("prob_equipo2"))
                    )),
      shiny::column(width = 6,
                    shiny::plotOutput(ns("plot_heatmap")))
    ),
  )
  
}