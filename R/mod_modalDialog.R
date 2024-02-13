# ouvre une fenetre, parametre renseigne quoi afficher dans la fenetre

#' @title Predefined modal
#'
#' @description Displays of formatted modal-dialog with 'Cancel' and 
#' 'Ok' buttons.
#' 
#' @param id A `character(1)` which is the id of the instance of the module
#' @param title A `character(1)`
#' @param width A `character(1)` indicating the size of the modal window. Can 
#' be "s" for small (the default), "m" for medium, or "l" for large.
#' @param uiContent The content of the modal dialog.
#'
#' @importFrom shinyjqui jqui_draggable
#' 
#' @name modalDialog
#' 
#' @return A Shiny modal-dialog
#'
NULL

#'
#' @rdname modalDialog
#'
#' @export
#'
modalDialog_ui <- function(id){
  # create the namespace from the id
  ns <- NS(id)
  
  #  As this is a function, the last statement will be the return value.
  #  Make sure it contains all of the UI elements you want to display
  
  fluidPage(
    fluidRow(
      actionButton(ns("show"), "Show modal dialog",
                   icon("chart-bar", lib = "font-awesome"),
                   class = "btn-success"),
      verbatimTextOutput(ns("dataInfo"))
    )
  )
}


#' @export
#'
#' @rdname modalDialog
#' 
#' @import shiny
#' @import shinyBS
#' @import shinyjqui
#'
modalDialog_server <- function(id,
                           title = NULL,
                           width = NULL,
                           uiContent = NULL,
                           external_mod = NULL,
                           external_mod_args = list()){ #height auto
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # reactiveValues object for storing current data set.
    dataOut <- reactiveVal(NULL)
    tmp <- reactiveVal(NULL)
    
    # Show modal when button is clicked.
    observeEvent(input$show, {
     req(external_mod)
      if (is.null(width)) {
        width <- "small"
      }
      
      tagList(
        tags$head(tags$style(paste0(".modal-dialog { width:", width, " }"))),
        tags$head(tags$style(".modal-dialog {z-index: 1000;}")),
        tags$head(
          tags$style("#test .modal-dialog {width: fit-content !important;}")),
        showModal(
        modalDialog(do.call(paste0(external_mod, '_ui'),
                            list(id = ns('test'))),
                    footer = tagList(
                      modalButton("Cancel"),
                      actionButton(ns("ok"), "OK2") # wrapped in ns()
                    )
                    
        )
      )
      )
    })
    
    observe({
      req(external_mod)
      args <- list(id = 'test')
      if (length(external_mod_args))
        args <- list(args, external_mod_args)
        
      tmp(do.call(paste0(external_mod, '_server'), args))
    })
    
    # When OK button is pressed, attempt to load the data set. If successful,
    # remove the modal. If not show another modal, but this time with a failure
    # message.
    observeEvent(input$ok, {
      dataOut(tmp()())
      removeModal()
    })
    
    
    return(reactive({dataOut()}))
  })
}



# Example
#
library(shiny)

ui <- fluidPage(
  modalDialog_ui(id = "tbl")
)

server <- function(input, output) {
  
  res <- modalDialog_server(id = "tbl",
                 title = "test modalDialog",
                 uiContent = p("test"),
                 external_mod = 'simple_mod',
                 external_mod_args = list()
                 )
  
  observeEvent(req(res()), {
    print(res())
  })
}

shinyApp(ui = ui, server = server)



# # Example in a module
# #
# library(shiny)
# library(shinyBS)
# 
# 
# loadapp_ui <- function(id){
#   ns <- NS(id)
#   uiOutput(ns('tutu'))
# }
# 
# loadapp_server <- function(id){
#   moduleServer(id, function(input, output, session){
#     ns <- session$ns
#     
#     
#     output$tutu <- renderUI({
#       modalDialog_ui(id = ns("tbl"))
#     })
#     
#     modalDialog_server(id = "tbl",
#                    title = "test2",
#                    uiContent = p("test2")
#     )
#   })
# }
# 
# 
# 
# ui <- fluidPage(
#   loadapp_ui(id = "tbl1")
# )
# 
# server <- function(input, output) {
#   
#   loadapp_server(id = "tbl1")
# }
# 
# shinyApp(ui = ui, server = server)
# 
