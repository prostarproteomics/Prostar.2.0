#' @title   mod_loading_page_ui and mod_loading_page_server
#' @description  A shiny Module.
#' 
#' @name mod_loading_page
#' 
#' @examples 
#' if(interactive(){
#' library(shinyjs)
#' library(shiny)
#' ui <- mod_loading_page_ui("mod_info")
#' server <- function(input, output, session) {
#'   mod_loading_page_server("mod_info")
#' }
#' 
#' shinyApp(ui, server)
#' }
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


#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_loading_page
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList
#' @importFrom shinyjs inlineCSS
mod_loading_page_ui <- function(id){
  ns <- NS(id)
  tagList(
      shinyjs::useShinyjs(),
      shinyjs::hidden(uiOutput(ns('splashscreen'))),
      #shinyjs::hidden(uiOutput(ns('load_pkg_UI')))
      uiOutput(ns('load_pkg_UI'))
  )
}

# Module Server

#' @param id xxx
#' @rdname mod_loading_page
#' @export
#' @keywords internal

mod_loading_page_server <- function(id, 
                                    pkg = NULL,
                                    funcs = NULL){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    done = mod_load_package_server('load_pkg', pkg = NULL, funcs = funcs)
    
    
    observeEvent(done(), {
      print(paste0('rv$done = ', done()))
      shinyjs::toggle('splashscreen', condition = isTRUE(done()))
      shinyjs::toggle('load_pkg_UI', condition = !isTRUE(done()))
    })
    
    
   
      
    
    output$load_pkg_UI <- renderUI({
      
      mod_load_package_ui(ns('load_pkg'))
      
    })
    
    
    
    output$splashscreen <- renderUI({
      tagList(
        h3('toto'),
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
    
    
  })
  
}



progressBar_css <- ".progress {
  position: relative;
  height: 4px;
  display: block;
  width: 100%;
  background-color: #E87352;
  border-radius: 2px;
  background-clip: padding-box;
  margin: 0.5rem 0 1rem 0;
  overflow: hidden; }
  
  
.progress .determinate {
    position: absolute;
    background-color: inherit;
    top: 0;
    bottom: 0;
    background-color: #E87352;
    transition: width .3s linear; }
    
.progress .indeterminate {
    background-color: blue; }
    .progress .indeterminate:before {
      content: '';
      position: absolute;
      background-color: #ffffff;
      top: 0;
      left: 0;
      bottom: 0;
      will-change: left, right;
      -webkit-animation: indeterminate 2.1s cubic-bezier(0.65, 0.815, 0.735, 0.395) infinite;
              animation: indeterminate 2.1s cubic-bezier(0.65, 0.815, 0.735, 0.395) infinite; }
    
.progress .indeterminate:after {
      content: '';
      position: absolute;
      background-color: #ffffff;
      top: 0;
      left: 0;
      bottom: 0;
      will-change: left, right;
      -webkit-animation: indeterminate-short 2.1s cubic-bezier(0.165, 0.84, 0.44, 1) infinite;
              animation: indeterminate-short 2.1s cubic-bezier(0.165, 0.84, 0.44, 1) infinite;
      -webkit-animation-delay: 1.15s;
              animation-delay: 1.15s; }

@-webkit-keyframes indeterminate {
  0% {
    left: -35%;
    right: 100%; }
  60% {
    left: 100%;
    right: -90%; }
  100% {
    left: 100%;
    right: -90%; } }
@keyframes indeterminate {
  0% {
    left: -35%;
    right: 100%; }
  60% {
    left: 100%;
    right: -90%; }
  100% {
    left: 100%;
    right: -90%; } }
@-webkit-keyframes indeterminate-short {
  0% {
    left: -200%;
    right: 100%; }
  60% {
    left: 107%;
    right: -8%; }
  100% {
    left: 107%;
    right: -8%; } }
@keyframes indeterminate-short {
  0% {
    left: -200%;
    right: 100%; }
  60% {
    left: 107%;
    right: -8%; }
  100% {
    left: 107%;
    right: -8%; } }"


#___________________________________________________________
ui <- mod_loading_page_ui("mod_pkg")

server <- function(input, output, session) {
  funcs <- c('Convert', 
             'mod_open_dataset', 
             'mod_open_demoDataset',
             'mod_view_dataset', 
             'mod_insert_md')
  
  done <- mod_loading_page_server("mod_pkg", funcs = funcs)
  #done <- mod_load_package_server("mod_pkg", pkg = 'DaparToolshed')
  

}

shinyApp(ui, server)


