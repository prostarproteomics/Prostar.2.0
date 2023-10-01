#' @title   mod_open_dataset_ui and mod_open_dataset_server
#' 
#' @description  A shiny Module.
#' 
#' @param id xxx
#' 
#' @name generic_mod_open_dataset
#'
#' @keywords internal
#' 
#' @examples 
#' if (interactive()){
#' ui <- fluidPage(
#' tagList(
#'   mod_open_dataset_ui("qf_file"),
#'   textOutput('res')
#' )
#' )
#' 
#' server <- function(input, output, session) {
#'   rv <- reactiveValues(
#'     obj = NULL
#'   )
#'   rv$obj <- mod_open_dataset_server("qf_file")
#'   
#'   output$res <- renderText({
#'     rv$obj()
#'     paste0('Names of the datasets: ', names(rv$obj()))
#'   })
#' }
#' 
#' shinyApp(ui, server)
#' }
#' 
NULL




#' @export 
#' @rdname generic_mod_open_dataset
#' @import shiny
#' 
default_mod_open_dataset_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3(style="color: blue;", '-- Default open dataset module --')
  )
}


#' @rdname generic_mod_open_dataset
#' 
#' @export
#' @importFrom shinyjs info
#' 
default_mod_open_dataset_server <- function(id){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rv.open <- reactiveValues(
      dataRead = NULL,
      dataOut = NULL
    )
    reactive({rv.open$dataOut})
  })
  
}






#----------------------------------------------------

ui <- default_mod_open_dataset_ui("qf_file")


server <- function(input, output, session) {
  default_mod_open_dataset_server("qf_file")

}

shinyApp(ui, server)
