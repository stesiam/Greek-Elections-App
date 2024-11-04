library(shiny)
library(shiny.tailwind)
library(shiny.i18n)
library(dplyr)
library(cookies)
library(htmltools)
library(highcharter)

source("global.R")


i18n <- Translator$new(translation_csvs_path = "www/translations")
i18n$set_translation_language("en")

# options(shiny.launch.browser = .rs.invokeShinyWindowExternal)

# Define UI for application that draws a histogram using HTML divs and tailwind
ui <- add_cookie_handlers(
  div(
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
            htmltools::tagAppendAttributes(selectInput('selected_language',width = "10em",
                        label = "",
                        choices = c("English" = "en", "Ελληνικά" = "el"),
                        selected = i18n$get_key_translation()),
                        
                        readonly = "", .cssSelector = "input")
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
  
  tags$div(
    class = "flex flex-wrap justify-around gap-6 px-4 py-8",
    
    # Card 1 - Analytics (with number)
    tags$div(
      class = "card w-full sm:w-80 md:w-64 bg-base-100 shadow-xl",
      tags$div(
        class = "card-body",
        tags$div(
          class = "flex items-center",
          tags$i(class = "fas fa-chart-line text-2xl text-blue-600"),  # FontAwesome icon
          tags$h2(class = "card-title ml-4", i18n$t("Percentage Difference"))
        ),
        tags$p(i18n$t("Between 1st and 2nd party")),
        tags$div(
          class = "text-4xl text-center font-bold text-blue-600 mt-4",
          textOutput("diff_first_second")  # Number to display
        )
      )
    ),
    
    # Card 2 - Parliament Parties
    tags$div(
      class = "card w-full sm:w-80 md:w-64 bg-base-100 shadow-xl",
      tags$div(
        class = "card-body",
        tags$div(
          class = "flex items-center",
          tags$i(class = "fas fa-users text-2xl text-green-600"),  # FontAwesome icon
          tags$h2(class = "card-title ml-4", i18n$t("Parliament Parties"))
        ),
        tags$p(i18n$t("Number of parties to elect MPs")),
        tags$div(
          class = "text-4xl text-center font-bold text-green-600 mt-4",
          textOutput("parties_elect_mps")  # Number to display
        )
      )
    ),
    
    # Card 3 - Settings (with number)
    tags$div(
      class = "card w-full sm:w-80 md:w-64 bg-base-100 shadow-xl",
      tags$div(
        class = "card-body",
        tags$div(
          class = "flex items-center",
          tags$i(class = "fa-solid fa-users-slash text-2xl text-red-600"),  # FontAwesome icon
          tags$h2(class = "card-title ml-4", i18n$t("Out of Parliament"))
        ),
        tags$p(i18n$t("Parties below 3% plus unallocated percentage")),
        tags$div(
          class = "text-4xl text-center font-bold text-red-600 mt-4",
          textOutput("outOfParliament")  # Number to display
        )
      )
    ),
    
    # Card 4
    tags$div(
      class = "card w-full sm:w-80 md:w-64 bg-base-100 shadow-xl",
      tags$div(
        class = "card-body",
        tags$div(
          class = "flex items-center",
          tags$i(class = "fas fa-cog text-2xl text-red-600"),  # FontAwesome icon
          tags$h2(class = "card-title ml-4", i18n$t("Unallocated Percentage"))
        ),
        tags$p(i18n$t("This should not be negative")),
        tags$div(
          class = "text-4xl text-center font-bold text-red-600 mt-4",
          textOutput("unallocatedPercentage")  # Number to display
        )
      )
    )
  ),
  div(class = "py-1 max-w-2xl mx-auto",
      column(width = 12,  
  div(class = "card flex justify-center max-w-2xl bg-base-100",
      tags$div(
        class = "flex p-2 justify-center items-center",
        tags$i(class = "fa-solid fa-users-slash text-2xl text-red-600"),  # FontAwesome icon
        tags$h2(class = "card-title ml-4", i18n$t("Parliament Overview"))
      ),
      highchartOutput("my_chart")
  ))),
  
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
                            tags$th(i18n$t("New Democracy"),
                                    tags$div(class = "established",
                                             tags$div(
                                               icon("user-tie")
                                             ),
                                             tags$div(
                                               tags$p(i18n$t("Mitsotakis K.")))
                                    ),
                                    tags$div(class = "established",
                                             tags$div(
                                               icon("calendar-days")
                                             ),
                                              tags$div(
                                                tags$p("1974"))
                                             )
                                    ),
                            tags$th(tags$input(id = "nd_pct", value =28.3, type="text", placeholder="Type here", class="input input-bordered input-primary w-full max-w-xs")),
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
                            tags$th(i18n$t("SYRIZA"),
                                    tags$div(class = "established",
                                             tags$div(
                                               icon("user-tie")
                                             ),
                                             tags$div(
                                               tags$p(i18n$t("Kasselakis S.")))
                                    ),
                                    tags$div(class = "established",
                                             tags$div(
                                               icon("calendar-days")
                                             ),
                                             tags$div(
                                               tags$p("2004"))
                                    )),
                            tags$th(tags$input(id = "syriza_pct", value =14.9, type="text", placeholder="Type here", class="input input-bordered input-primary w-full max-w-xs")),
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
                            tags$th(i18n$t("PASOK"),
                                    tags$div(class = "established",
                                             tags$div(
                                               icon("user-tie")
                                             ),
                                             tags$div(
                                               tags$p(i18n$t("Androulakis N.")))
                                    ),
                                    tags$div(class = "established",
                                             tags$div(
                                               icon("calendar-days")
                                             ),
                                             tags$div(
                                               tags$p("1974"))
                                    )),
                            tags$th(tags$input(id = "pasok_pct", value =12.8, type="text", placeholder="Type here", class="input input-bordered input-primary w-full max-w-xs")),
                            tags$th(textOutput("pasok_seats"))
                          ),
                          tags$tr(
                            tags$th(
                              div(class="avatar",
                                  div(class="mask mask-squircle w-12 h-12",
                                      tags$img(src="ellisi_logo.png", alt="KKE logo"),
                                  )
                              )
                            ),
                            tags$th(i18n$t("Greek Solution"),
                                    tags$div(class = "established",
                                             tags$div(
                                               icon("user-tie")
                                             ),
                                             tags$div(
                                               tags$p(i18n$t("Velopoulos K.")))
                                    ),
                                    tags$div(class = "established",
                                             tags$div(
                                               icon("calendar-days")
                                             ),
                                             tags$div(
                                               tags$p("2016"))
                                    )),
                            tags$th(tags$input(id = "ellisi_pct", value =9.3, type="text", placeholder="Type here", class="input input-bordered input-primary w-full max-w-xs")),
                            tags$th(textOutput("ellisi_seats"))
                          ),
                          tags$tr(
                            tags$th(
                              div(class="avatar",
                                  div(class="mask mask-squircle w-12 h-12",
                                      tags$img(src="kke_logo.png", alt="KKE logo"),
                                  )
                              )
                            ),
                            tags$th(i18n$t("KKE"),
                                    tags$div(class = "established",
                                             tags$div(
                                               icon("user-tie")
                                             ),
                                             tags$div(
                                               tags$p(i18n$t("Koutsoumpas D.")))
                                    ),
                                    tags$div(class = "established",
                                             tags$div(
                                               icon("calendar-days")
                                             ),
                                             tags$div(
                                               tags$p("1918"))
                                    )),
                            tags$th(tags$input(id = "kke_pct", value =9.25, type="text", placeholder="Type here", class="input input-bordered input-primary w-full max-w-xs")),
                            tags$th(textOutput("kke_seats"))
                          ),
                          tags$tr(
                            tags$th(
                              div(class="avatar",
                                  div(class="mask mask-squircle w-12 h-12",
                                      tags$img(src="niki_logo.png", alt="Plefsi logo"),
                                  )
                              )
                            ),
                            tags$th(i18n$t("Niki"),
                                    tags$div(class = "established",
                                             tags$div(
                                               icon("user-tie")
                                             ),
                                             tags$div(
                                               tags$p(i18n$t("Natsios D.")))
                                    ),
                                    tags$div(class = "established",
                                             tags$div(
                                               icon("calendar-days")
                                             ),
                                             tags$div(
                                               tags$p("2019"))
                                    )
                                    ),
                            tags$th(tags$input(id = "niki_pct", value =4.37, type="text", placeholder="Type here", class="input input-bordered input-primary w-full max-w-xs")),
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
                            tags$th(i18n$t("Course of Freedom"),
                                    tags$div(class = "established",
                                             tags$div(
                                               icon("user-tie")
                                             ),
                                             tags$div(
                                               tags$p(i18n$t("Konstantopoulou Z.")))
                                    ),
                                    tags$div(class = "established",
                                             tags$div(
                                               icon("calendar-days")
                                             ),
                                             tags$div(
                                               tags$p("2016"))
                                    )),
                            tags$th(tags$input(id = "plefsi_pct", value =3.4,type="text", placeholder="Type here", class="input input-bordered input-primary w-full max-w-xs")),
                            tags$th(textOutput("plefsi_seats"))
                          ),
                          tags$tr(
                            tags$th(
                              div(class="avatar",
                                  div(class="mask mask-squircle w-12 h-12"
                                  )
                              )
                            ),
                            tags$th(i18n$t("Foni Logikis"),
                                    tags$div(class = "established",
                                             tags$div(
                                               icon("user-tie")
                                             ),
                                             tags$div(
                                               tags$p(i18n$t("Latinopoulou A.")))
                                    ),
                                    tags$div(class = "established",
                                             tags$div(
                                               icon("calendar-days")
                                             ),
                                             tags$div(
                                               tags$p("2023"))
                                    )),
                            tags$th(tags$input(id = "foni_pct", value =3.04, type="text", placeholder="Type here", class="input input-bordered input-primary w-full max-w-xs")),
                            tags$th(textOutput("foni_seats"))
                          ),
                          tags$tr(
                            tags$th(
                              div(class="avatar",
                                  div(class="mask mask-squircle w-12 h-12",
                                      tags$img(src="mera25.jpeg", alt="Plefsi logo"),
                                  )
                              )
                            ),
                            tags$th(i18n$t("MeRA25"),
                                    tags$div(class = "established",
                                             tags$div(
                                               icon("user-tie")
                                             ),
                                             tags$div(
                                               tags$p(i18n$t("Varoufakis Y.")))
                                    ),
                                    tags$div(class = "established",
                                             tags$div(
                                               icon("calendar-days")
                                             ),
                                             tags$div(
                                               tags$p("2018"))
                                    )),
                            tags$th(tags$input(id = "mera25_pct", value =2.54, type="text", placeholder="Type here", class="input input-bordered input-primary w-full max-w-xs")),
                            tags$th(textOutput("mera_seats"))
                          ),
                          tags$tr(
                            tags$th(
                              div(class="avatar",
                                  div(class="mask mask-squircle w-12 h-12",
                                      tags$img(src="nar_logo.svg", alt="Nea Aristera logo"),
                                  )
                              )
                            ),
                            tags$th(i18n$t("New Left"),
                                    tags$div(class = "established",
                                             tags$div(
                                               icon("user-tie")
                                             ),
                                             tags$div(
                                               tags$p(i18n$t("Haritsis A.")))
                                    ),
                                    tags$div(class = "established",
                                             tags$div(
                                               icon("calendar-days")
                                             ),
                                             tags$div(
                                               tags$p("2024"))
                                    )),
                            tags$th(tags$input(id = "nar_pct", value =2.45, type="text", placeholder="Type here", class="input input-bordered input-primary w-full max-w-xs")),
                            tags$th(textOutput("nar_seats"))
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
      "Copyright ©", tags$a(class = "link link-primary", href="https://www.stesiam.com/", target="_blank", "stesiam,"), "2024")
  )
)
)
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ## Set cookies to remember language selection
  
  observeEvent(
    input$selected_language,
    {
      set_cookie(
        cookie_name = "lang_cookie",
        cookie_value = input$selected_language
      )
    }
  )
  
  observeEvent(
    get_cookie("lang_cookie"),
    updateSliderInput(
      inputId = "selected_language",
      value = get_cookie("lang_cookie")
    ),
    once = TRUE
  )
  
  ## End of cookies

  default_selected <- reactiveVal("propbonus")
  
  
observeEvent(input$selected_language, {
  output$tab_content <- renderUI({
    htmltools::tagAppendAttributes(selectInput(
      inputId = "elect_systems",
      label = "",
      width = "280px",
      choices = c(setNames(c('bonus40'), i18n$t("Bonus 40 Seats (2007-2009)")),
                  setNames(c('bonus50'), i18n$t("Bonus 50 Seats (2012-2019)")),
                  setNames(c('nobonus'), i18n$t("No Bonus (2023Α)")),
                  setNames(c('propbonus'), i18n$t("Proportional Bonus (2023Β-"))
    ),
    selected = default_selected()),
    
    readonly = "", .cssSelector = "input")
  })
})

    data = reactive({
     raw_data = data.frame(
       "Party" = c("ND", "SYRIZA", "PASOK","KKE","Ellisi", "Niki", "Plefsi", 
                   "Mera", "Other1", "Other2", "Foni Logikis", "Nea Aristera"),
       "FullParty" = c("New Democracy", "Coalition of the Radical Left", "Panhellenic Socialistic Movement",
                       "Communist Party of Greece", "Greek Solution", "Victory", "Course of Freedom",
                       "Mera25 - DieM25", "Other Party #1", "Other Party #2",
                       "Voice of Reason", "New Left"),
       "Perc" = as.numeric(c(input$nd_pct, input$syriza_pct, input$pasok_pct, input$kke_pct,
                  input$ellisi_pct, input$niki_pct, input$plefsi_pct, input$mera25_pct,
                  input$other1_pct, input$other2_pct, input$foni_pct, input$nar_pct)),
       "col" = c("#008AC5", "#FFC0CB", "#64A12D", "#EB001F",
                 "#608AC5", "#CC7722",  "#BE3075","#E34234",
                 "#808588", "#777B7E", "#808AC5", "#ff3050"),
       "orientation" = c(7,5,6, 2, 9,8,3,4, 1,10,9.5,4.5)
        )
    })
    
    data_without_NAs = reactive({
      data() |>
        mutate(Perc = ifelse(is.na(Perc), 0, Perc))
    })
    
    
    
    calc_seats = reactive({
      req(input$elect_systems) 
      data = data_without_NAs() 
      
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
    output$foni_seats = renderText({ calc_seats()$IntSeats[[11]] })
    output$nar_seats = renderText({ calc_seats()$IntSeats[[12]] })
    
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
        
        observeEvent(data_without_NAs(), {
          output$parties_elect_mps <- renderText({
            parties_elect_mps(data_without_NAs())
          })
        })
        
        observeEvent(data_without_NAs(), {
          output$outOfParliament <- renderText({
            paste0(round((100 - inparliament(data_without_NAs())), digits =2), "%")
          })
        })
        
        observeEvent(data_without_NAs(), {
          output$unallocatedPercentage <- renderText({
            paste0(round(unallocated(data_without_NAs()), digits =2), "%")
          })
        })
        
        
        observeEvent(data_without_NAs(), {
          output$diff_first_second <- renderText({
            paste0(round(find_difference(data_without_NAs()), digits =2), "%")
          })
        })
        
        output$my_chart <- renderHighchart({
         w =  data_without_NAs() |>
            mutate(
              Seats = calc_seats()$IntSeats
            ) %>%
           dplyr::filter(Seats >0) %>%
           arrange(orientation)
         
         
         
            hchart(
              w,
              "item",
              hcaes(
                name = Party,
                y = Seats,
                label = FullParty,
                color = col),
              name = "Representatives",
              showInLegend = TRUE,
              size = "125%",
              center = list("50%", "75%"),
              startAngle = -100,
              endAngle  = 100
        ) %>%
          hc_legend(labelFormat = '{FullParty} <span style="opacity: 0.4">{y}</span>')
        })

}

# Run the application 
shinyApp(ui = ui, server = server)
