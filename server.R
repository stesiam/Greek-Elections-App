server <- function(input, output) {
  
  inparl = reactive({
    x = c(input$nd, input$syriza, input$pasok,
    input$kke, input$ellisi, input$mera25,
    input$niki, input$plefsi, input$other1)
    inparl = 0
    toallocate = 100
    for (i in 1:length(x)){
      if( x[i]>= 3)
        inparl = inparl + x[i]
        toallocate = toallocate - x[i]
      }
    return(list(inparl, toallocate))
  })

  output$nd_seats <- renderText({ nobonus(input$nd, inparl()[[1]]) })
  output$syriza_seats <- renderText({  nobonus(input$syriza, inparl()[[1]])   })
  output$pasok_seats <- renderText({  nobonus(input$pasok, inparl()[[1]])  })
  output$kke_seats <- renderText({  nobonus(input$kke, inparl()[[1]])  })
  output$ellisi_seats <- renderText({  nobonus(input$ellisi, inparl()[[1]])  })
  output$niki_seats <- renderText({ nobonus(input$niki, inparl()[[1]])  })
  output$plefsi_seats <- renderText({ nobonus(input$plefsi, inparl()[[1]]) })
  output$mera25_seats <- renderText({  nobonus(input$mera25, inparl()[[1]])  })
  output$other1_seats <- renderText({ nobonus(input$other1, inparl()[[1]]) })
  output$other2_seats <- renderText({ nobonus(input$other2, inparl()[[1]]) })
  
  output$toallocate_server <- renderText({ inparl()[[2]] })
  output$oop_server <- renderText({ 100 - inparl()[[1]] })
}