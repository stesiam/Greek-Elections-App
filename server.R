server <- function(input, output) {
  output$nd_seats <- renderText({ floor(input$nd/0.5) })
  output$syriza_seats <- renderText({ floor(input$syriza/0.5) })
  output$pasok_seats <- renderText({ floor(input$pasok/0.5) })
  output$kke_seats <- renderText({ floor(input$kke/0.5) })
  output$ellisi_seats <- renderText({ floor(input$ellisi/0.5) })
  output$mera25_seats <- renderText({ floor(input$mera25/0.5) })
  output$other1_seats <- renderText({ floor(input$other1/0.5) })
  output$other2_seats <- renderText({ floor(input$other2/0.5) })
  output$other3_seats <- renderText({ floor(input$other3/0.5) })
}