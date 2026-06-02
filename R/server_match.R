server_match = function(id){
  shiny::moduleServer(id,function(input,output,session){

    ns = session$ns

    ##########################################################################
    ################## OBSERVE

    shiny::observeEvent(input$button_match,{
      showModal(modal_match)
    })

    modal_match = shiny::modalDialog(
      title = "Confirm Match",
      footer = shiny::tagList(
        shiny::div(
          shiny::actionButton(ns("cancel_button"),"Cancel", class = "btn btn-sucess"),
          shiny::actionButton(ns("add_button"),"Add", class = "btn btn-sucess")
        )
      )
    )

    shiny::observeEvent(input$add_button, {

      Hist_Data = leer_historial()

      add = data.frame(equipo1= input$equipo1_match,
                       equipo2 = input$equipo2_match,
                       goles1 = input$equipo1_goles,
                       goles2 = input$equipo2_goles,
                       Grupo = NA)

      Hist_Data = rbind(Hist_Data,add)

      saveRDS(Hist_Data, file = "Data/Hist_Data.RDS")

      shiny::removeModal()

      shiny::showModal(shiny::modalDialog(
        title = "Exito",
        "Data Addes succesfully",
        easyClose = TRUE,
        footer = NULL
      ))

    })

    shiny::observeEvent(input$cancel_button,{
      shiny::removeModal()
    })





  })
}
