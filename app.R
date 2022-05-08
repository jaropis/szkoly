#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(magrittr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    h1("Szkoly"),
    fluidRow(
        column(9,
            selectizeInput(inputId = "wojewodztwo", label = "Województwo", choices = NULL, selected = NULL),
            selectizeInput(inputId = "typ", label = "Typ placówki", choices = NULL, selected = NULL, multiple = TRUE)
        )
    ),
    
    fluidRow(
        column(9,
               textOutput(outputId = "maile")
               )
    )
    # Sidebar with a slider input for number of bins 
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    library(tibble)
    
    szkoly <- reactive({as_tibble(read.csv("wykaz_szkol.csv"))})
    wojewodztwa <- reactive({unique(szkoly()[["Województwo"]])})
    rodzaj <- reactive({unique(szkoly()[["Nazwa.typu"]])})
    maile <- reactiveVal({"pusto"})
    observeEvent(szkoly, {
        updateSelectizeInput(session = session, inputId = "wojewodztwo", choices = wojewodztwa())
        updateSelectizeInput(session = session, inputId = "typ", choices = rodzaj())
    })
    
    observe({
        req(input$wojewodztwo)
        req(input$typ)
        maile_inside <- szkoly()[["e.mail"]]
        maile_inside[szkoly()[["Województwo"]] %in% input$wojewodztwo & szkoly()[["Nazwa.typu"]] %in% input$typ] %>% 
            maile()
    })
    
    output$maile <- renderText(maile())
    
}

# Run the application 
shinyApp(ui = ui, server = server)
