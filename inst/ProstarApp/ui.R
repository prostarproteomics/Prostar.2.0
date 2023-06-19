library(shinydashboard)
library(shinyjs)



#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboardPlus
#' @import shinydashboard
#' @noRd
shinyUI(
    tagList(

        #launchGA(),
        shinyjs::useShinyjs(),
        shinyjs::extendShinyjs(text = "shinyjs.resetProstar = function() {history.go(0)}",
            functions = c("resetProstar")),
        
        #theme = "css/ceruleanProstar.css",
        #theme = shinythemes::shinytheme("cerulean"),
        
        titlePanel("", windowTitle = "Prostar"),
        div(id = "loading_page", mod_loading_page_ui('loadPage') ),
        shinyjs::hidden(div(id='main_page', mod_main_page_ui('mainPage')))
    )
)

