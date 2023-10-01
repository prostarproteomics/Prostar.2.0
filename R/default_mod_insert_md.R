# Module UI

#' @title   mod_insert_md_ui and mod_insert_md_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param url xxx
#' 
#' @name generic_mod_insert_md
#' 
NULL


#' @rdname generic_mod_insert_md
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' 
default_mod_insert_md_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3(style="color: blue;", '-- Default insert markdown module --'),
    uiOutput(ns("insertMD"))
  )
}

# Module Server

#' @rdname generic_mod_insert_md
#' @export
#' @keywords internal

default_mod_insert_md_server <- function(id, url){
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    
    output$insertMD <- renderUI({
      tryCatch(
        {
          includeMarkdown(url)
        }
        , warning = function(w) {
          tags$p("URL not found<br>",conditionMessage(w))
        }, error = function(e) {
          shinyjs::info(paste("URL not found :", conditionMessage(e), sep=" "))
        }, finally = {
          #cleanup-code 
        })
      
    })
    
    
  })
  
}



###################################################################
#                             Example                             #
###################################################################

library(shiny)

ui <- shinyUI(
  fluidPage(
    default_mod_insert_md_ui(id = 'ex')
  )
)

server <- function(input, output) {
  
  url <- "http://www.prostar-proteomics.org/md/presentation.md"
  default_mod_insert_md_server("ex", url)
  
}

shinyApp(ui = ui, server = server)


