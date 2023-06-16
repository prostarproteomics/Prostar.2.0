#' @title   mod_choose_pipeline_ui and mod_choose_pipeline_server
#' @description  A shiny Module.
#' 
#' @param id shiny id
#'
#' 
#' @name mod_Convert
#' @author Samuel Wieczorek
#' 
NULL


#' @rdname mod_Convert
#' @export
#' 
generic_Convert_conf <- function(){
  
}



#' @rdname mod_Convert
#'
#' @keywords internal
#' @export
#'
#' @import shiny
#' @import shiny
#' @return NA
#'
generic_Convert_ui <- function(id) {
  ns <- NS(id)
  h3('Generic convert module')
}



#' @export
#' @import shiny
#' @rdname mod_Convert
#'
generic_Convert_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })

}



#---------------------------------------------

ui <- generic_Convert_ui("qf_file")


server <- function(input, output, session) {
  generic_Convert_server("qf_file")
  
}

shinyApp(ui, server)


