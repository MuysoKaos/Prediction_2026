main_ui = function(id){

  ns = shiny::NS(id)

  bslib::page_navbar(
    theme = bslib::bs_theme(bootswatch = "lux"),
    title = "Predicciones Mundial",
    bslib::nav_panel("Prediccion", icon = shiny::icon("fas fa-chart-pie"), tab_predict("predictor_mundial")),
    bslib::nav_panel("Add_match", icon = shiny::icon("fas fa-chart-pie"), tab_match("predictor_mundial"))
  )


}

main_server = function(){

  server_predict("predictor_mundial")
  server_match("predictor_mundial")

}

predictor_mundial = function(){
  shiny::shinyApp(
    shiny::shinyUI(main_ui("predictor_mundial")),
    server = function(input,output,session){
      main_server()
    }
  )

}
