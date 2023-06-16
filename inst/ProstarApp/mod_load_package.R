
#' @title   mod_choose_pipeline_ui and mod_choose_pipeline_server
#' @description  This module is used to load the package which contains all the code
#' to work with prostar.2.0.
#' 
#' @param id shiny id
#' @param pkg xxx
#'
#' 
#' @name mod_load_package
#' @author Samuel Wieczorek
#' 
NULL

timeoutSeconds <- 30*60

inactivity <- sprintf("function idleTimer() {
var t = setTimeout(logout, %s);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions
function logout() {
Shiny.setInputValue('timeOut', '%ss')
}
function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, %s);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();", timeoutSeconds*1000, timeoutSeconds, timeoutSeconds*1000)



#' @rdname mod_load_package
#' @import shiny
#' @export
#'
mod_load_package_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    shinyjs::hidden(uiOutput(ns('splashscreen'))),
    shinyjs::hidden(uiOutput(ns('ui')))
  )
}





#'
#' @export
#' @import shiny
#' @rdname mod_load_package
#'
mod_load_package_server <- function(id, pkg = NULL) {
  
  modules <- c('Convert_ui', 'Convert_server',
               'mod_open_dataset_ui', 'mod_open_dataset_server',
               'mod_open_demoDataset_ui', 'mod_open_demoDataset_server',
               'mod_view_dataset_ui', 'mod_view_dataset_server',
               'generic_mod_insert_md_ui', 'generic_mod_insert_md_server')
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv.pkg <- reactiveVal(pkg)
    dataOut <- reactiveVal(NULL)
    
    
    observe({
      print('test')
      shinyjs::toggle('splashscreen', condition = !is.null(pkg))
      shinyjs::toggle('ui', condition = is.null(pkg))
    })
    
    output$splashscreen <- renderUI({
      tagList(
        tags$script(inactivity),
      shinyjs::inlineCSS(progressBar_css),
      absolutePanel(
        id  = ns("AbsolutePanel"),
        class = "panel panel-default",
        style= "text-align: center; background-color: #25949A;",
        top = '30%',
        left = '25%',
        width = "50%",
        height = "150px",
        draggable = FALSE,
        fixed = TRUE,
        tagList(
          tags$h1(style='text-align: center; color: white', "Prostar is loading, please wait..."),
          br(),
          tags$div(class="progress",
            tags$div(class="indeterminate")
          )
        )
        )
      )
    })
      
      
      
    output$ui <- renderUI({
      req(is.null(pkg))
      tagList(
        selectInput(ns('choose_pkg'), 'Choose package',
                      choices = rownames(installed.packages())),
        actionButton(ns('load_pkg'), 'Load package')
        )
    })
    
    
    observeEvent(input$load_pkg, {rv.pkg(input$choose_pkg)})
    
    observe({
      req(rv.pkg())
      
      library(rv.pkg(),character.only = TRUE)
      pkg.lst <- ls(paste0('package:', rv.pkg()))
      if ('Convert_ui' %in% pkg.lst){
        Convert_ui <- eval(parse(text = paste0(rv.pkg(),"::Convert_ui")))
        Convert_server <- eval(parse(text = paste0(rv.pkg(),"::Convert_server")))
        Convert_conf <- eval(parse(text = paste0(rv.pkg(),"::Convert_conf")))
      } else {
        Convert_ui <- generic_Convert_ui
        Convert_conf <- generic_Convert_conf
        Convert_server <- generic_Convert_server
      }
      
      
      if ('mod_open_dataset_ui' %in% pkg.lst){
        mod_open_dataset_ui <- eval(parse(text = paste0(rv.pkg(),"::mod_open_dataset_ui")))
        mod_open_dataset_server <- eval(parse(text = paste0(rv.pkg(),"::mod_open_dataset_server")))
      } else {
        mod_open_dataset_ui <- generic_mod_open_dataset_ui
        mod_open_dataset_server <- generic_mod_open_dataset_server
      }
      
      
      if (('mod_open_demoDataset_ui' %in% pkg.lst) &&
          ('mod_open_demoDataset_server' %in% pkg.lst)){
        mod_open_demoDataset_ui <- eval(parse(text = paste0(rv.pkg(),"::mod_open_demoDataset_ui")))
        mod_open_demoDataset_server <- eval(parse(text = paste0(rv.pkg(),"::mod_open_demoDataset_server")))
      } else {
        mod_open_demoDataset_ui <- generic_mod_open_demoDataset_ui
        mod_open_demoDataset_server <- generic_mod_open_demoDataset_server
      }
      
      
      
      if (('mod_view_dataset_ui' %in% pkg.lst) &&
          ('mod_view_dataset_server' %in% pkg.lst)){
        mod_view_dataset_ui <- eval(parse(text = paste0(rv.pkg(),"::mod_view_dataset_ui")))
        mod_view_dataset_server <- eval(parse(text = paste0(rv.pkg(),"::mod_view_dataset_server")))
      } else {
        mod_view_dataset_ui <- generic_mod_view_dataset_ui
        mod_view_dataset_server <- generic_mod_view_dataset_server
      }
      
      
      if (('mod_insert_md_ui' %in% pkg.lst) &&
          ('mod_insert_md_server' %in% pkg.lst)){
        mod_insert_md_ui <- eval(parse(text = paste0(rv.pkg(),"::mod_insert_md_ui")))
        mod_insert_md_server <- eval(parse(text = paste0(rv.pkg(),"::mod_insert_md_server")))
      } else {
        mod_insert_md_ui <- generic_mod_insert_md_ui
        mod_insert_md_server <- generic_mod_insert_md_server
      }
      
      print("done")
      dataOut(TRUE)
    })
    
    reactive({dataOut()})
  })
  
}



#___________________________________________________________
ui <- mod_load_package_ui("mod_pkg")

server <- function(input, output, session) {
  done <- mod_load_package_server("mod_pkg")
  #done <- mod_load_package_server("mod_pkg", pkg = 'DaparToolshed')
  observeEvent(req(done()), {
    
    print(done())
  })
}

shinyApp(ui, server)

