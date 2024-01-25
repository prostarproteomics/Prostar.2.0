# Module UI

#' @title   mod_main_page_ui and mod_loading_page_server
#' @description  A shiny Module.
#' 
#' @name mod_main_page
#' 
#' @examples 
#' if(interactive()){
#' library(shinyjs)
#' library(shiny)
#' library(shinydashboard)
#' 
#' 
#' ui <- mod_main_page_ui("mod_info")
#' 
#' server <- function(input, output, session) {
#'   mod_main_page_server("mod_info")
#' }
#' 
#' shinyApp(ui, server)
#' 
#' }
#' 
NULL


#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_main_page
#'
#' @keywords internal
#' @export 
#' @import shiny
#' @import shinyjs
#' @import shinydashboard
#' @import shinydashboardPlus
#' @import shinyEffects
#' @import MagellanNTK
#' 
mainapp_ui <- function(id){
  ns <- NS(id)
  
  div(id = "header",
    
      shinydashboardPlus::dashboardPage(
        md = FALSE,
        skin="blue",
        
        #skin = shinythemes::shinytheme("cerulean"),
        
        # https://stackoverflow.com/questions/31711307/how-to-change-color-in-shiny-dashboard
        # orangeProstar <- "#E97D5E"
        # gradient greenblue header
        # greenblue links <- #2fa4e7
        # darker greenblue hover links <- #157ab5
        # darker greenblue titles <- #317eac
        # small titles <- #9999
        # darkest greenblue button reset+next+selected menu
        # color background arrow : #88b7d5 (bleu gris clair)
        # lightgrey #dddd
        # grey #ccc
        # bleu ceruleen #2EA8B1
        # jaune clair 'mark' #FCF8E3
        # green #468847
        # darker green #356635
        
        ##
        ## Header
        ## 
        # header = dashboardHeader(
        #   fixed = TRUE,
        #   title = dashboardthemes::shinyDashboardLogo(theme = "blue_gradient",
        #                                               boldText = "Prostar",
        #                                               badgeText = "v2"),
        #   leftUi = tagList(
        #   actionButton('browser', 'Console'),
        #   a(href="http://www.prostar-proteomics.org/"
        #       # img(src=base64enc::dataURI(
        #       #   file=system.file('ProstarApp/www/images', 'LogoProstarComplet.png', package='ProstarDev'), 
        #       #   mime="image/png"))
        #       ),
        #    a(href="https://github.com/samWieczorek/Prostar2",
        #       icon("github"),
        #       title="GitHub")
        #   )
        # ),
        dashboardHeader(
          fixed = TRUE,
          # titleWidth = "245px",
          # title = absolutePanel(
          #    fixed = TRUE,
          #    height = '100px',
          #    dashboardthemes::shinyDashboardLogo(theme = "blue_gradient",
          #                                        boldText = "Prostar",
          #                                        badgeText = "v2")
          #    ),
          # leftUi = tagList(
          #   tags$style(".skin-blue .main-header .navbar {background-color: rgb(20,97,117);}"),
          #   actionButton('browser', 'Console'),
          #   a(href="http://www.prostar-proteomics.org/"
          #     #       # img(src=base64enc::dataURI(
          #     #       #   file=system.file('ProstarApp/www/images', 'LogoProstarComplet.png', package='ProstarDev'), 
          #     #       #   mime="image/png"))
          #            ),
          #   a(href="https://github.com/prostarproteomics/Prostar.2.0",
          #     icon("github"),
          #     title="GitHub")
          # 
          # )
          title = 
            tagList(
              span(class = "logo-lg", 
                   absolutePanel(fixed = TRUE, "Menu"))
              #absolutePanel(fixed = TRUE,  img(src = "ShinyDashboardPlus_FINAL.svg"))
              ),
          leftUi = tagList(
            h4("Prostar"), shinydashboardPlus::dashboardBadge("2.0", color = "green"),
            
            
            # dropdownBlock(
            #   id = "mydropdown2",
            #   title = "Dropdown 2",
            #   icon = icon("sliders"),
            #   prettySwitch(
            #     inputId = "switch4",
            #     label = "Fill switch with status:",
            #     fill = TRUE, 
            #     status = "primary"
            #   ),
            #   prettyCheckboxGroup(
            #     inputId = "checkgroup2",
            #     label = "Click me!", 
            #     thick = TRUE,
            #     choices = c("Click me !", "Me !", "Or me !"),
            #     animation = "pulse", 
            #     status = "info"
            #   )
            # )
          ),
          
          # dropdownMenu(
          #   type = "tasks", 
          #   badgeStatus = "danger",
          #   taskItem(value = 20, color = "aqua", "Refactor code"),
          #   taskItem(value = 40, color = "green", "Design new layout"),
          #   taskItem(value = 60, color = "yellow", "Another task"),
          #   taskItem(value = 80, color = "red", "Write documentation")
          # ),
          userOutput(ns("user"))
        ),
        ##
        ## Sidebar
        ## 
        sidebar = dashboardSidebar(
          #fixed = TRUE,
          sidebarMenu(id = "sb",
                      #style = "position: fixed; overflow: visible;",
            # inactiveClass for import menus inactivation 
           # tags$head(tags$style(".inactiveLink {pointer-events: none; background-color: grey;}")),
            
            # Menus and submenus in sidebar
            #br(),
            menuItem("Home", 
                     tabName = "ProstarHome", 
                     icon = icon("home"),
                     selected = TRUE),
            hr(),
            menuItem("Data Manager",
                     icon = icon("folder"),
                     startExpanded = TRUE,
                     menuSubItem("Open QFeature file", 
                                 tabName = "openFile"),
                     menuSubItem("Convert Data", 
                                 tabName = "convert"),
                     menuSubItem("Demo data", 
                                 tabName = "demoData"),
                     menuSubItem("Export Results", 
                                 tabName = "export")
                     ),
            hr(),
            menuItem("Pipeline", 
                     tabName = "pipeline", 
                     icon = icon("cogs"),
                     badgeLabel = "todo", 
                     badgeColor = "blue"),
            hr(),
            menuItem("Dapar Viz", 
                     tabName = "daparviz", 
                     icon = icon("cogs"),
                     badgeLabel = "new", 
                     badgeColor = "green"),
            hr(),
            menuItem("Help", 
                     icon = icon("question-circle"),
                     menuSubItem("Useful Links", 
                                 tabName = "usefulLinks"),
                     menuSubItem("FAQ", 
                                 tabName = "faq"),
                     menuSubItem("Bug Report", 
                                 tabName = "bugReport"),
                     menuSubItem("Global Settings", 
                                 tabName = "globalSettings", 
                                 icon = icon("cogs")),
                     menuSubItem("Release Notes", 
                                 tabName = "releaseNotes", 
                                 icon = icon("clipboard")),
                     menuSubItem("Check for Updates", 
                                 tabName = "checkUpdates", 
                                 icon = icon("wrench"))
                     )
            )
        ),
        controlbar = dashboardControlbar(
          skin = "dark",
          controlbarMenu(
            controlbarItem(
              title = "Tab 1",
              icon = icon("desktop"),
              active = TRUE,
              actionLink('browser', 'Console')
            ),
            controlbarItem(
              icon = icon("paint-brush"),
              title = "Change skin",
              shinydashboardPlus::skinSelector()
            )
          )
        ),
        body = dashboardBody(
          
          # some styling
          tags$head(
            tags$style(
              rel = "stylesheet",
              type = "text/css",
              href = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/qtcreator_dark.min.css"
            ),
            tags$script(
              src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js"
            ),
            tags$script(
              "$(function() {
            $('.sidebar-toggle').on('click', function() {
              $('.skinSelector-widget').toggle();
            });
          });
          "
            )
          ),
          
          #dashboardthemes::shinyDashboardThemes(
          #  theme = "blue_gradient"
          #),

          tagList(
            #tags$head(
            #  tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
            #),
            
            shinyjs::useShinyjs(),
            
            # body content
            tabItems(
              tabItem(tabName = "ProstarHome", class="active",
                mod_homepage_ui(ns('home'))
              ),
              tabItem(tabName = "openFile", uiOutput(ns('open_dataset_UI'))),
              tabItem(tabName = "convert", uiOutput(ns('open_convert_dataset_UI'))),
              tabItem(tabName = "demoData", uiOutput(ns('open_demo_dataset_UI'))),
              
              tabItem(tabName = "daparviz", uiOutput(ns('EDA_UI'))),
              
              tabItem(tabName = "export", h3("Export")), # export module not yet
              tabItem(tabName = "globalSettings", mod_settings_ui(ns('global_settings'))),
              tabItem(tabName = "releaseNotes", mod_release_notes_ui(ns('rl'))),
              tabItem(tabName = "checkUpdates", mod_check_updates_ui(ns('check_updates'))),
              tabItem(tabName = "usefulLinks", insert_md_ui(ns('links_MD'))),
              tabItem(tabName = "faq", insert_md_ui(ns('FAQ_MD'))),
              tabItem(tabName = "bugReport", mod_bug_report_ui(ns("bug_report"))),
              tabItem(tabName = "pipeline", uiOutput(ns('show_pipeline')))
            )
            # uiOutput('show_pipeline')
          )
        )
      )
      # mod_navbar_menu_ui('mainMenu')
      
      #                   fluidPage(
      #                     navbarPage("Prostar",
      #                     position = "fixed-top",
      #                     id = "navPage",
      #                     inverse = FALSE,
      # 
      #                     
      #                     
      #                     #modulePlotsUI('showPlots')
      #                     navbarMenu("Prostar",
      #                                tabPanel(title="Home",
      #                                         value="HomeTab",mod_homepage_ui("homepage")),
      #                                tabPanel(title="Global settings",
      #                                         value="GlobalSettingsTab", mod_settings_ui("modSettings")),
      #                                tabPanel("Release notes",
      #                                         value="ReleaseNotesTab",mod_release_notes_ui("modReleaseNotes")),
      #                                tabPanel("Check for updates",
      #                                         value="CheckUpdatesTab",mod_check_updates_ui("modCheckUpdates"))
      #                     ),
      #                     navbarMenu("Data manager",
      #                                tabPanel("Open MSnset",value = 'openMSnsetTab',
      #                                         mod_open_dataset_ui('moduleOpenDataset'),
      #                                         mod_infos_dataset_ui("infos_openFile")
      #                                         ),
      #                                tabPanel("Convert",value = "convertTab",
      #                                         mod_convert_ms_file_ui('moduleProcess_Convert')
      #                                         ),
      #                                tabPanel("Demo data",  value='demoTab', 
      #                                         mod_open_demo_dataset_ui('mod_OpenDemoDataset'),
      #                                         mod_infos_dataset_ui("infos_demoDataset")
      #                                         ),
      #                                tabPanel(title="ReloadProstar",
      #                                          value="ReloadTab",
      #                                          p("Due to some instability of cache memory when successively opening several datasets in a Prostar session, data management has been simplified.
      #                                           To work on another dataset than the current one, reloading Prostar first is now necessary (with the button above).  It will restart Prostar
      #                                           with a fresh R session where import menus are enabled 'Dataset manager' menu."),
      #                                          actionButton("ReloadProstar", "Reload Prostar",class = actionBtnClass)
      #                                           )
      #                     ),
      #                     # navbarMenu("Data mining",
      #                     #            tabPanel("Descriptive statistics", value='descriptiveStats', mod_all_plots_ui('modAllPlots'))
      #                     # ),
      #                     navbarMenu("Help",
      #                                tabPanel("Links",value="usefulLinksTab",  mod_insert_md_ui('links_MD')),
      #                                tabPanel("FAQ", value="faqTab",  mod_insert_md_ui('FAQ_MD')),
      #                                tabPanel("Bug report",value="bugReportTab",  mod_bug_report_ui('bugreport')
      # 
      #                     )
      #                     )
      #                   ) ## end navbarPage
      # )
    )

}

# Module Server

#' @param id xxx
#' @rdname mod_main_page
#' @export
#' @keywords internal
#' @import shinydashboard
#' @import MagellanNTK
mainapp_server <- function(id,
                           funcs = NULL){
  
  source(system.file("ProstarApp/global.R", package = 'Prostar.2.0'))$value
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rv.core <- reactiveValues(
      pipeline = NULL,
      pipeline.name = NULL,
      dataIn = NULL,
      result_convert = NULL,
      result_openDemoDataset = NULL,
      result_open_dataset = NULL,
      # Current QFeatures object in Prostar
      current.obj = NULL,
      
      # pipeline choosen by the user for its dataset
      current.pipeline = NULL
    )
    
    
    delay(ms = 3500, show("app_title"))
    delay(ms = 3800, show("app_slider_plot"))
    
    
    
    output$user <- renderUser({
      dashboardUser(
        name = "Prostar proteomics", 
        #image = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg", 
        image = system.file('inst/ProstarApp/www/logo-simple.png', package='Prostar.2.0'), 
        #title = "Prostar-proteomics",
        #subtitle = "Author", 
        footer = fluidRow(
          column(width = 6, socialButton(
          href = "https://github.com/prostarproteomics/Prostar.2.0",
          icon = icon("github")
        )),
        column(
          width = 6,
          socialButton(
            href = "https://prostar-proteomics.org",
            icon = icon("dropbox")
          )
          )),
        p('TODO: Describe the web site')
      )
    })
    
    
    observeEvent(input$ReloadProstar, {
      js$resetProstar()
    })
    
    
    
    
    
    #rv.core$pipeline.name <- choose_pipeline_server('pipe', package = 'MSPipelines')
    
    
    
    
    #
    # Code for convert tool
    #
    rv.core$result_convert_dataset <- call.func(
      fname = paste0(funcs$convert, '_server'),
      args = list(id = 'Convert'))
    
    output$open_convert_dataset_UI <- renderUI({
      req(funcs)
      call.func(
        fname = paste0(funcs$convert, '_ui'),
        args = list(id = ns('Convert')))
    })
    
     observeEvent(req(rv.core$result_convert_dataset()),{
       rv.core$current.obj <- rv.core$result_convert_dataset()
       
        #rv.core$current.pipeline <- rv.core$tmp_dataManager$openFile()$pipeline
     })
    
    
    #
    # Code for open demo dataset
    #
    rv.core$result_demo_dataset <- call.func(
      fname = paste0(funcs$open_demoDataset, '_server'),
      args = list(id = 'open_demo_dataset'))
    
    output$open_demo_dataset_UI <- renderUI({
      req(funcs)
      call.func(
        fname = paste0(funcs$open_demoDataset, '_ui'),
        args = list(id = ns('open_demo_dataset')))
    })
    
    observeEvent(req(rv.core$result_demo_dataset()),{
      rv.core$current.obj <- rv.core$result_demo_dataset()
      print(rv.core$current.obj)
      # #rv.core$current.pipeline <- rv.core$tmp_dataManager$openFile()$pipeline
    })
    
    # observe({
    #   req(funcs)
    #   rv.core$current.obj <- do.call(
    #     eval(parse(text=paste0(funcs$open_demoDataset, '_server'))),
    #     args = list(id = ns('demo_data')))
    #   #rv.core$current.pipeline <- rv.core$tmp_dataManager$openFile()$pipeline
    #   print('rv.core$current.obj has changed')
    # })
     
    
    
     #
     # Code for open dataset
     #
    rv.core$result_open_dataset <- call.func(
      fname = paste0(funcs$open_dataset, '_server'),
      args = list(id = 'open_dataset'))

     output$open_dataset_UI <- renderUI({
       req(funcs)
       call.func(fname = paste0(funcs$open_dataset, '_ui'),
                 args = list(id = ns('open_dataset')))
     })

    observeEvent(req(rv.core$result_open_dataset()),{
     rv.core$current.obj <- rv.core$result_open_dataset()
     print(rv.core$current.obj)
   # #rv.core$current.pipeline <- rv.core$tmp_dataManager$openFile()$pipeline
    })
    
    
    #rv.core$result_convert <- Convert_server('Convert')
    
     #rv.core$result_convert <- nav_server(id = 'Convert',
     #                                     dataIn = reactive({data.frame()}))
     
     
    #observeEvent(rv.core$result_convert$dataOut()$trigger,{
      #browser()
     # rv.core$dataIn <- rv.core$result_convert$dataOut()$value
      #   rv.core$current.pipeline <- rv.core$tmp_dataManager$convert()$pipeline
   # })
    
    # observe({
    #   #shinyjs::toggle('div_demoDataset', condition = !is.null(rv.core$pipeline.name()) && rv.core$pipeline.name() != 'None')
    #   shinyjs::toggle('load_dataset_btn', condition = !is.null(rv.core$result_openDemoDataset()))
    # })
    # 
    observeEvent(input$browser,{browser()})
    
    # observe({
    #   req(rv.core$pipeline.name() != 'None')
    #   print("Launch Magellan")
    #   obj <- base::get(rv.core$pipeline.name())
    #   rv.core$pipeline <- do.call(obj$new, list('App'))
    #   rv.core$pipeline$server(dataIn = reactive({rv.core$dataIn}))
    # })
    
    # observeEvent(input$load_dataset_btn, {
    #   #browser()
    #   print(names(rv.core$result_openDemoDataset()))
    #   updateTabItems(session, "sb", "pipeline")
    #   shinyjs::delay(100, rv.core$dataIn <- rv.core$result_openDemoDataset())
    # })
    
    
    observeEvent(input$ReloadProstar, { js$reset()})
    
    
    #output$show_convert <- renderUI({
    #  req(convert)
    #  convert$ui()
    #})
    
    
    # https://github.com/daattali/shinyjs/issues/74
    #output$show_pipeline <- renderUI({
    #  req(rv.core$pipeline)
    #  rv.core$pipeline$ui()
    # if (!is.null(rv.core$dataIn))
    #   rv.core$pipeline$ui()
    # else
    #   shinyjs::disabled(rv.core$pipeline$ui())
    #})
    
    
    # mimics loading data > body content and inactivation of import menus in sidebar
    # observeEvent(rv.core$current.pipeline, ignoreNULL=FALSE, { 
    #   #https://stackoverflow.com/questions/48278111/disable-enable-click-on-dashboard-sidebar-in-shiny
    #   
    #   if(is.null(rv.core$current.pipeline)){
    #     # show sidebar and button sidebar
    #     shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    #     shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'visible';")
    #     
    #     # enable import menus
    #     shinyjs::removeCssClass(selector = "a[data-value='openFile']", class = "inactiveLink")
    #     shinyjs::removeCssClass(selector = "a[data-value='convert']", class = "inactiveLink")
    #     shinyjs::removeCssClass(selector = "a[data-value='demoData']", class = "inactiveLink")
    #   }
    #   else{ # "after data loaded"
    #     # hide sidebar/button sidebar
    #     shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    #     shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")
    #     
    #     # disable import menus
    #     shinyjs::addCssClass(selector = "a[data-value='openFile']", class = "inactiveLink")
    #     shinyjs::addCssClass(selector = "a[data-value='convert']", class = "inactiveLink")
    #     shinyjs::addCssClass(selector = "a[data-value='demoData']", class = "inactiveLink")
    #   } 
    # })
    
    #browser()
    

      call.func(
        fname = paste0(funcs$view_dataset, '_server'),
        args = list(id = 'view_dataset',
                    obj = reactive({DaparViz::convert2Viz(rv.core$current.obj)})))
    
    #---------------------------Server modules calls---------------------------------------------------#
    output$EDA_UI <- renderUI({
      req(funcs)
      call.func(
        fname = paste0(funcs$view_dataset, '_ui'),
        args = list(id = ns('view_dataset')))
    })
    
    
    #mod_test_server('tutu')
    mod_homepage_server('home')
    #mod_settings_server("global_settings", obj = reactive({Exp1_R25_prot}))
  mod_release_notes_server("rl")
    mod_check_updates_server("check_updates")
    insert_md_server("links_MD", 
                         URL_links)
    insert_md_server("FAQ_MD", 
                     URL_FAQ,
                     link_URL = 'https://www.prostar-proteomics.org/#Frequently_asked_questions')
    #mod_bug_report_server("bug_report")
  })
  
}





#___________________________________________________________
ui <- fluidPage(
  mainapp_ui("main")
)

server <- function(input, output, session) {
  funcs <- list(convert = "DaparToolshed::convert",
                open_dataset = "DaparToolshed::open_dataset",
                open_demoDataset = "DaparToolshed::open_demoDataset",
                view_dataset = "DaparViz::view_dataset"
  )
  
  mainapp_server("main", funcs = funcs)
}

shinyApp(ui, server)


