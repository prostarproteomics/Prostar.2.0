
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
    shinyjs::hidden(div(id = ns('div_checkUi'), DT::DTOutput(ns('checkUi')))),
    shinyjs::hidden(uiOutput(ns('ui')))
  )
}





#'
#' @export
#' @import shiny
#' @rdname mod_load_package
#'
mod_load_package_server <- function(id, pkg = NULL, funcs = NULL) {
  
 
 
  
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv.pkg <- reactiveVal(pkg)
    dataOut <- reactiveVal(NULL)
    rv <- reactiveValues(
      list.funcs = NULL
      )
   
    observe({
      shinyjs::toggle('splashscreen', condition = !is.null(pkg))
      shinyjs::toggle('div_checkUi', condition = is.null(pkg))
      shinyjs::toggle('ui', condition = is.null(pkg))
    })
    
    
    observeEvent(req(funcs), {
      rv$list.funcs <- lapply(setNames(nm=funcs), function(x) NULL)
      target.pkgs()
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
      
    # is.ok <- function(test, lst.funcs){
    #   tt <- lapply(test, function(x) grep(x, lst.funcs, value = TRUE))
    #   return(length(unlist(tt))==length(test))
    # }
    
    target.pkgs <- function(){
      
      for (candidate in rownames(installed.packages())){
        #print(candidate)
        #browser()
        lst.funcs <- NULL
        tryCatch({
          lst.funcs <- getNamespaceExports(candidate)
        },
        warning = function(w) {return(NULL)},
        error = function(e) {return(NULL)})
        
        #2#print(lst.funcs)
        
        #if (sum(c('Convert_ui', 'Convert_server', 'Convert_conf') %in% lst.funcs) == 3)
        #  rv$list.funcs$Convert <- c(rv$list.funcs$Convert, candidate)
        
        isolate({
        for (mod in names(rv$list.funcs))
          if (sum(paste0(mod, c('_ui', '_server')) %in% lst.funcs) == 2)
            rv$list.funcs[[mod]] <- c(rv$list.funcs[[mod]], candidate)
        })
        #if (candidate == 'DaparToolshed')
        #  browser()
        
        
      }

      rv$list.funcs
    }
      
    
    output$checkUi <- DT::renderDataTable({
      req(rv$list.funcs)
#browser()
      mods <- names(rv$list.funcs)
      pkgs <- unique(unname(unlist(rv$list.funcs)))
      n <- length(names(rv$list.funcs))
      m = matrix(
        names(rv$list.funcs), nrow = length(rv$list.funcs), ncol = length(pkgs), byrow = TRUE,
        dimnames = list(names(rv$list.funcs), pkgs)
      )
      
      for (i in seq_len(nrow(m))) {
        if (!is.null(rv$list.funcs[[i]])){
          for(j in pkgs) {
            if ( j %in% rv$list.funcs[[i]])
              m[i, j] = sprintf('<input type="radio" name="%s" value="%s"/>', names(rv$list.funcs)[i], m[i, j])
         else 
          m[i, j] = sprintf('')
        }
        }
      }
      
      DT::datatable(m,
                escape = FALSE, 
                options = list(
                  dom = 't', 
                  paging = FALSE, 
                  ordering = FALSE)
                )

    })
    
    
    
    # output$ui <- renderUI({
    #   req(c(is.null(pkg), target.pkgs()))
    #   #browser()
    #   tagList(
    #     selectInput(ns('choose_pkg'), 'Choose package',
    #                   choices = unique(unname(unlist(target.pkgs()))),
    #                 multiple = TRUE, 
    #                 #selectize = FALSE,
    #                 selected = '',
    #                 width='150px'),
    #     actionButton(ns('load_pkg'), 'Load package')
    #     )
    # })
    
    
    observeEvent(input$load_pkg, {rv.pkg(input$choose_pkg)})
    
    observe({
      req(rv.pkg())
      
      library(rv.pkg(), character.only = TRUE)
      pkg.lst <- ls(paste0('package:', rv.pkg()))
      if ('Convert_ui' %in% pkg.lst){
        print(paste0('load Convert funcs from ', rv.pkg()))
        Convert_ui <- eval(parse(text = paste0(rv.pkg(),"::Convert_ui")))
        Convert_server <- eval(parse(text = paste0(rv.pkg(),"::Convert_server")))
        Convert_conf <- eval(parse(text = paste0(rv.pkg(),"::Convert_conf")))
      } else {
        print('load generic_Convert funcs')
        Convert_ui <- generic_Convert_ui
        Convert_conf <- generic_Convert_conf
        Convert_server <- generic_Convert_server
      }
      
      
      if ('mod_open_dataset_ui' %in% pkg.lst){
        print(paste0('load mod_open_dataset funcs from ', rv.pkg()))
        mod_open_dataset_ui <- eval(parse(text = paste0(rv.pkg(),"::mod_open_dataset_ui")))
        mod_open_dataset_server <- eval(parse(text = paste0(rv.pkg(),"::mod_open_dataset_server")))
      } else {
        print('load generic_mod_open_dataset funcs')
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
  funcs <- c('Convert', 'mod_open_dataset', 'mod_open_demoDataset',
             'mod_view_dataset', 'mod_insert_md')
  
  done <- mod_load_package_server("mod_pkg", funcs = funcs)
  #done <- mod_load_package_server("mod_pkg", pkg = 'DaparToolshed')
  observeEvent(req(done()), {
    
    print(done())
  })
}

shinyApp(ui, server)

