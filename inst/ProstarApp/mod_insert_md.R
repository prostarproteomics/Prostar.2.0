# Module UI

#' @title   mod_insert_md_ui and mod_insert_md_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param url xxx
#' 
#' @name mod_insert_md
#' 
NULL


#' @rdname mod_insert_md
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' 
insert_md_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("insertMD"))
  )
}

# Module Server

#' @rdname mod_insert_md
#' @export
#' @keywords internal

insert_md_server <- function(id, url){
  
  
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
    insert_md_ui(id = 'ex')
  )
)

server <- function(input, output) {
  
  url <- "http://www.prostar-proteomics.org/md/presentation.md"
  insert_md_server("ex", url)
  
}

shinyApp(ui = ui, server = server)


