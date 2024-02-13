simple_mod_ui <- function(id){
  # create the namespace from the id
  ns <- NS(id)
  
  fluidPage(
    actionButton(ns("test"), "Test")
    )
}


simple_mod_server <- function(id){ #height auto
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # reactiveValues object for storing current data set.
    dataOut <- reactiveVal(NULL)

    observeEvent(input$test, {
      dataOut(paste0('Clicked ', input$test, ' times.'))
    })
    
    
    return(reactive({dataOut()}))
  })
}




# Example
#
library(shiny)

ui <- fluidPage(
  simple_mod_ui(id = "tbl")
)

server <- function(input, output) {
  
  simple_mod_server(id = "tbl")
}

shinyApp(ui = ui, server = server)

