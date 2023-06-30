options(shiny.maxRequestSize=300*1024^2,
        encoding = "UTF-8",
        shiny.fullstacktrace = T
        )
require(compiler)
enableJIT(3)


#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyjs
#' 
#' @noRd
shinyServer( 
  
    function(input, output, session ) {
      
      funcs <- c('Convert', 
                 'mod_open_dataset', 
                 'mod_open_demoDataset',
                 'mod_view_dataset', 
                 'mod_insert_md')
      done <- loadapp_server('loadapp_module', funcs = funcs)
      
      observeEvent(req(done()), {

      # Once the server part is loaded, hide the loading page 
      # and show th main content
        
      #  browser()
      shinyjs::hide('div_loadapp_module', anim = TRUE, animType = "fade", time=3)
      shinyjs::show('div_mainapp_module', anim = TRUE, animType = "fade", time=3)
      
      mainapp_server('mainapp_module')
      })

 
    }
)