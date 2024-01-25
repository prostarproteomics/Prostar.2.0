# Module UI

#' @title   mod_open_demo_dataset_ui and mod_open_demo_dataset_server
#' 
#' @description  A shiny Module.
#' 
#' @param id xxx
#' 
#' @name infos_dataset
#'
#' @keywords internal
#' 
NULL




#' @export 
#' @rdname infos_dataset
#' @importFrom shiny NS tagList 
#' @import shinyjs
#' @import DaparViz
#' 
infos_dataset_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3(style="color: blue;", '-- Default info dataset module --')
  )
}

# Module Server

#' @rdname info_dataset
#' 
#' @export
#' @importFrom BiocGenerics get
#' @importFrom utils data
#' @importFrom shinyjs info
#' 
infos_dataset_server <- function(id, 
                                 obj = reactive({NULL})){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    
  })
  
}




###################################################################
##                                                               ##
##                                                               ##
###################################################################

library(shiny)

ui <- infos_dataset_ui("infos")


server <- function(input, output, session) {
  rv <- reactiveValues(
    obj = NULL
  )
  
  infos_dataset_server("infos", obj = reactive({NULL}))
}

shinyApp(ui = ui, server = server)
