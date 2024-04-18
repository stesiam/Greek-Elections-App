library(shiny)
library(shiny.tailwind)
library(shiny.i18n)
library(dplyr)

source("global.R")


i18n <- Translator$new(translation_csvs_path = "www/translations/")
i18n$set_translation_language("en")

# options(shiny.launch.browser = .rs.invokeShinyWindowExternal)

# Define UI for application that draws a histogram using HTML divs and tailwind
ui <- div(
  tags$head(
    tags$meta(name = "viewport",
              content="width=device-width, initial-scale = 1"),
    usei18n(i18n),
    # Load Tailwind CSS Just-in-time
    use_daisyui(),
    includeCSS("www/styles.css")
  ),
  
  # Title
    div(class = "custom-navbar navbar bg-base-100 px-5",
        div(class = "flex-1",
            a(href = "#", class="btn btn-ghost text-xl", 
              tags$div(class = "title-comb", icon("check-to-slot"), tags$h1(i18n$t("Greek Elections App"))))),
        div(class = "flex-none gap-x-3",
            selectInput('selected_language',width = "10em",
                        label = "",
                        choices = c("English" = "en", "Ελληνικά" = "el"),
                        selected = i18n$get_key_translation())
            ),
        ),
  div(class = "wrapper-card",
  div(class = "custom-card",
      div(class = "card-title", i18n$t("Choose Electoral System: ")),
      div(class = "selector-electoral",
          div(class = "py-1 max-w-2xl mx-auto",
              uiOutput("tab_content")
          )
      )
  )
  ),
  
  
  div(class = "py-1 max-w-2xl mx-auto",
  column(width = 12,
         div(class = "overflow-x-auto  flex flex-col align-center justify-center",
             tags$table(class="table table-zebra d-flex",
                         tags$thead(
                           tags$tr(
                             tags$th(),
                             tags$th(i18n$t("Party")),
                             tags$th(i18n$t("Percentage")),
                             tags$th(i18n$t("Seats"))
                           )
                         ),
                        tags$tbody(
                          tags$tr(
                            tags$th(
                              div(class="avatar",
                                div(class="mask mask-squircle w-12 h-12",
                                tags$img(src="nd_logo.svg", alt="New Democracy Logo"),
                                )
                              )
                            ),
                            tags$th(i18n$t("New Democracy")),
                            tags$th(tags$input(id = "nd_pct", value =40.56, type="text", placeholder="Type here", class="input input-bordered input-primary w-full max-w-xs")),
                            tags$th(textOutput("nd_seats"))
                          ),
                          tags$tr(
                            tags$th(
                              div(class="avatar",
                                  div(class="mask mask-squircle w-12 h-12",
                                      tags$img(src="syriza_logo.png", alt="SYRIZA's party logo"),
                                  )
                              )
                            ),
                            tags$th(i18n$t("SYRIZA")),
                            tags$th(tags$input(id = "syriza_pct", value =17.83, type="text", placeholder="Type here", class="input input-bordered input-primary w-full max-w-xs")),
                            tags$th(textOutput("syriza_seats"))
                          ),
                          tags$tr(
                            tags$th(
                              div(class="avatar",
                                  div(class="mask mask-squircle w-12 h-12",
                                      tags$img(src="pasok_logo.png", alt="SYRIZA logo"),
                                  )
                              )
                            ),
                            tags$th(i18n$t("PASOK")),
                            tags$th(tags$input(id = "pasok_pct", value =11.84, type="text", placeholder="Type here", class="input input-bordered input-primary w-full max-w-xs")),
                            tags$th(textOutput("pasok_seats"))
                          ),
                          tags$tr(
                            tags$th(
                              div(class="avatar",
                                  div(class="mask mask-squircle w-12 h-12",
                                      tags$img(src="kke_logo.png", alt="KKE logo"),
                                  )
                              )
                            ),
                            tags$th(i18n$t("KKE")),
                            tags$th(tags$input(id = "kke_pct", value =7.69, type="text", placeholder="Type here", class="input input-bordered input-primary w-full max-w-xs")),
                            tags$th(textOutput("kke_seats"))
                          ),
                          tags$tr(
                            tags$th(
                              div(class="avatar",
                                  div(class="mask mask-squircle w-12 h-12",
                                      tags$img(src="ellisi_logo.png", alt="KKE logo"),
                                  )
                              )
                            ),
                            tags$th(i18n$t("Elliniki Lysi")),
                            tags$th(tags$input(id = "ellisi_pct", value =4.44, type="text", placeholder="Type here", class="input input-bordered input-primary w-full max-w-xs")),
                            tags$th(textOutput("ellisi_seats"))
                          ),
                          tags$tr(
                            tags$th(
                              div(class="avatar",
                                  div(class="mask mask-squircle w-12 h-12",
                                      tags$img(src="niki_logo.png", alt="Plefsi logo"),
                                  )
                              )
                            ),
                            tags$th(i18n$t("Niki")),
                            tags$th(tags$input(id = "niki_pct", value =3.7, type="text", placeholder="Type here", class="input input-bordered input-primary w-full max-w-xs")),
                            tags$th(textOutput("niki_seats"))
                          ),
                          tags$tr(
                            tags$th(
                              div(class="avatar",
                                  div(class="mask mask-squircle w-12 h-12",
                                      tags$img(src="plefsi_logo.png", alt="Plefsi logo"),
                                  )
                              )
                            ),
                            tags$th(i18n$t("Plefsi")),
                            tags$th(tags$input(id = "plefsi_pct", value =3.17,type="text", placeholder="Type here", class="input input-bordered input-primary w-full max-w-xs")),
                            tags$th(textOutput("plefsi_seats"))
                          ),
                          tags$tr(
                            tags$th(
                              div(class="avatar",
                                  div(class="mask mask-squircle w-12 h-12",
                                      tags$img(src="mera25.jpeg", alt="Plefsi logo"),
                                  )
                              )
                            ),
                            tags$th(i18n$t("MeRA25")),
                            tags$th(tags$input(id = "mera25_pct", value =2.5, type="text", placeholder="Type here", class="input input-bordered input-primary w-full max-w-xs")),
                            tags$th(textOutput("mera_seats"))
                          ),
                          tags$tr(
                            tags$th(
                              div(class="avatar",
                                  div(class="mask mask-squircle w-12 h-12"
                                  )
                              )
                            ),
                            tags$th(i18n$t("Other Party #1")),
                            tags$th(tags$input(id = "other1_pct", value =1, type="text", placeholder="Type here", class="input input-bordered input-primary w-full max-w-xs")),
                            tags$th(textOutput("other1_seats"))
                          ),
                          tags$tr(
                            tags$th(
                              div(class="avatar",
                                  div(class="mask mask-squircle w-12 h-12"
                                  )
                              )
                            ),
                            tags$th(i18n$t("Other Party #2")),
                            tags$th(tags$input(id = "other2_pct", value =1, type="text", placeholder="Type here", class="input input-bordered input-primary w-full max-w-xs")),
                            tags$th(textOutput("other2_seats"))
                          )
                        )
                   )
         )

         )),

  tags$footer(
    class = "footer footer-center p-4 bg-base-300 text-base-content",
    tags$div(class = "flex flex-row justify-center align-center", 
      tags$p(
      "Copyright ©", tags$a(class = "link link-primary", href="https://www.stesiam.com/", target="_blank", "stesiam"), ", ", "2024")
  )
)
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {

  default_selected <- reactiveVal("propbonus")
  
  
observeEvent(input$selected_language, {
  output$tab_content <- renderUI({
    selectInput(
      inputId = "elect_systems",
      label = "",
      width = "280px",
      choices = c(setNames(c('bonus40'), i18n$t("Bonus 40 Seats (2007-09)")),
                  setNames(c('bonus50'), i18n$t("Bonus 50 Seats (2012-2019)")),
                  setNames(c('nobonus'), i18n$t("No Bonus (2023Α)")),
                  setNames(c('propbonus'), i18n$t("Proportional Bonus (2023Β-"))
    ),
    selected = default_selected())
  })
})

    data = reactive({
     data.frame(
       "Party" = c("ND", "SYRIZA", "PASOK","KKE","Ellisi", "Niki", "Plefsi", "Mera", "Other1", "Other2"),
       "Perc" = c(input$nd_pct, input$syriza_pct, input$pasok_pct, input$kke_pct,
                  input$ellisi_pct, input$niki_pct, input$plefsi_pct, input$mera25_pct,
                  input$other1_pct, input$other2_pct)
     )
    })
    
    
    calc_seats = reactive({
      req(input$elect_systems) 
      data = data() 
      
      if (input$elect_systems == "bonus40") {
        data %>%
          mutate(Perc = as.numeric(Perc)) %>%
          bonus40()
      } else if (input$elect_systems == "bonus50") {
        data %>%
          mutate(Perc = as.numeric(Perc)) %>%
          bonus50()
      } else if (input$elect_systems == "nobonus") {
        data %>%
          mutate(Perc = as.numeric(Perc)) %>%
          nobonus()
      } else{
        data %>%
          mutate(Perc = as.numeric(Perc)) %>%
          calculate_seats()
      }
      
    })
    

    output$nd_seats = renderText({ calc_seats()$IntSeats[[1]] })
    output$syriza_seats = renderText({ calc_seats()$IntSeats[[2]] })
    output$pasok_seats = renderText({ calc_seats()$IntSeats[[3]] })
    output$kke_seats = renderText({ calc_seats()$IntSeats[[4]] })
    output$ellisi_seats = renderText({ calc_seats()$IntSeats[[5]] })
    output$niki_seats = renderText({ calc_seats()$IntSeats[[6]] })
    output$plefsi_seats = renderText({ calc_seats()$IntSeats[[7]] })
    output$mera_seats = renderText({ calc_seats()$IntSeats[[8]] })
    output$other1_seats = renderText({ calc_seats()$IntSeats[[9]] })
    output$other2_seats = renderText({ calc_seats()$IntSeats[[10]] })
        
    
    observeEvent(input$selected_language, {
          shiny.i18n::update_lang("el", session)
          output$message <- renderText({
            "Greek language selected!"
          })
        })
        
        observeEvent(input$selected_language, {
          # This print is just for demonstration
          print(paste("Language change!", input$selected_language))
          # Here is where we update language in session
          shiny.i18n::update_lang(input$selected_language)
        })
        
        
        observeEvent(input$enButton, {
          shiny.i18n::update_lang(session, input$enButton)
          output$message <- renderText({
            "Eng language selected!"
          })
        })
        
      
}

# Run the application 
shinyApp(ui = ui, server = server)
