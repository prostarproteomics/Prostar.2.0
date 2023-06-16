# Module UI

#' @title   mod_open_demo_dataset_ui and mod_open_demo_dataset_server
#' 
#' @description  A shiny Module.
#' 
#' @param id xxx
#' 
#' @name generic_mod_open_demo_dataset
#'
#' @keywords internal
#' 
NULL




#' @export 
#' @rdname generic_mod_open_demo_dataset
#' @importFrom shiny NS tagList 
#' @import shinyjs
#' @import DaparViz
#' 
generic_mod_open_demoDataset_ui <- function(id){
  ns <- NS(id)
  tagList(
   h3('Generic mod open demoDataset module')
  )
}

# Module Server

#' @rdname generic_mod_open_demo_dataset
#' 
#' @export
#' @importFrom BiocGenerics get
#' @importFrom utils data
#' @importFrom shinyjs info
#' 
generic_mod_open_demoDataset_server <- function(id){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rv.openDemo <- reactiveValues(
      dataRead = NULL,
      dataOut = NULL
    )
    
    reactive({rv.openDemo$dataOut })
  })
  
}




###################################################################
##                                                               ##
##                                                               ##
###################################################################

library(shiny)

ui <- generic_mod_open_demoDataset_ui("demo")


server <- function(input, output, session) {
  rv <- reactiveValues(
    obj = NULL
  )
  
  rv$obj <- generic_mod_open_demoDataset_server("demo")

}

shinyApp(ui = ui, server = server)
