library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(RColorBrewer)

# Add www subdirectory for images and stuff
addResourcePath(prefix = 'www', directoryPath = paste0(getwd(), '/www'))

# We'll do a dynamic redirect after authenticating
# https://stackoverflow.com/questions/57755830/how-to-redirect-to-a-dynamic-url-in-shiny
jscode <- "Shiny.addCustomMessageHandler('customredirect', function(message) { window.location = message;});"

# Plot color palettes
PLOT_COLORS <- brewer.pal(11, 'Spectral')
PLOT_SHAPES <- 21:25

titlify <- function(s){
  to_title <- function(ss){
    ss = paste0(toupper(substr(ss,1,1)), substr(ss,2,nchar(ss)))
    ss = gsub('_', ' ', ss)
    ss = gsub(' ic', ' IC', ss)
    return(ss)
  }
  if (length(s) > 1){
    return(sapply(s, to_title))
  } else{
    return(to_title(s))
  }
}

# ------------------------------- Pre-login UI ------------------------------ #

AnonymousUI <- fluidPage(
  tags$head(tags$link(rel="stylesheet", type="text/css", href="bootstrap_rani.css"),
            HTML('<link rel="icon" href="favicon.ico" type="image/x-icon"/>')),
  title = 'Transcriptomics Explorer',
  
  h1('Transcriptomics Explorer'),
  
  # DNA
  HTML('<div id="dna">
    <div></div><div></div><div></div><div></div><div></div>
    <div></div><div></div><div></div><div></div><div></div>
    <div></div><div></div><div></div><div></div><div></div>
    <div></div><div></div><div></div><div></div><div></div>
    <div></div><div></div><div></div><div></div><div></div>
    <div></div><div></div><div></div><div></div><div></div>
    </div>'),
  
  # Login button
  tags$head(tags$script(jscode)),
  div(actionButton("action", "Log in with Synapse"),
      style='text-align: center; padding-bottom: 50px;'),
  
  div(p("Don't have a Synapse account yet?", style='text-align: center; font-size: 8;'),
      p("Follow the ",
        a(href = 'https://midas-wyss.github.io/synapse_instructions_predictive_bioanalytics.html',
          target = '_blank', 'instructions for registering here'), 
        '.', style='text-align: center;'))
)

# ------------------------------ Post-login UI ------------------------------ #

AuthenticatedUI <- dashboardPage(
  skin = 'blue',
  
  dashboardHeader(title = "Transcriptomics Explorer",
                  titleWidth = 250,
                  tags$li(class = "dropdown",
                          tags$li(class = "dropdown", 
                                  actionLink("user_account_modal", textOutput("logged_user")),
                                  style = "color: #fff;"))),
  
  dashboardSidebar(
    width = 250,
    tags$head(tags$style(HTML('.logo {
                              background-color: #3aa4a9 !important;
                              }
                              .navbar {
                              background-color: #42B5BB !important;
                              }
                              .sidebar-toggle {
                              background-color: #42B5BB !important;
                              }
                              .skin-blue .sidebar-menu > li.active > a,
                              .skin-blue .sidebar-menu > li:hover > a { border-left-color: #3aa4a9;}'
    ))),
    selectInput('project_select', 
                label = 'Project',
                selected = 'Influenza biomarkers',
                choices = c('Influenza biomarkers')),
    div(actionLink('info_modal', 'What are Projects?', 
                   style = 'color: #42B5BB;'), 
        style = 'font-size: 8pt; margin: 0px 5px 20px 0px;'),
    sidebarMenu(
      menuItem("Samples", tabName = "tab_samples", icon = icon("vial")),
      menuItem("Differential expression", tabName = "tab_diff_expr", icon = icon("chart-bar")),
      menuItem("Pathway enrichment", tabName = "tab_pathway_enrichment", icon = icon("align-left")),
      menuItem("Biomarkers", tabName = "tab_biomarkers", icon = icon("share-alt")),
      menuItem("DRUID", tabName = "tab_druid", icon = icon("star"))
    ),
    br(),
    div(p('This tool is maintained by the Predictive BioAnalytics group - Wyss Institute at Harvard',
          style = 'margin: 60px 5px 6px 15px;'),
        style = 'font-size: 8pt; margin: 0px 5px 20px 0px;'),
    div(a('Contact us!', href = 'https://midas-wyss.github.io/', 
          style = 'color: #42B5BB; margin: 6px 5px 6px 15px;'),
        style = 'font-size: 8pt; margin: 0px 5px 20px 0px;')
  ),
  
  dashboardBody(
    tags$head(includeHTML('www/analytics.html'),
              tags$style(".shiny-output-error{color: white;}"),
              HTML('<link rel="icon" href="favicon.ico" type="image/x-icon"/>')),
    tabItems(
      tabItem(tabName = "tab_samples",
              fluidRow(
                box(title = "Between-sample variability (UMAP algorithm)", 
                    width = 7,
                    height = 500,
                    p(style='padding-left: 20px; color: #D2D6DD;', 'Hover over points to view sample info'),
                    div(withSpinner(plotlyOutput('umap_plot'),
                                    type = 4, color = '#42B5BB'),
                        style = "overflow-y: auto;")),
                box(title = "UMAP plot settings",
                    width = 5,
                    height = 500,
                    div(style = 'padding: 0 30px 20px 30px; float: left;',
                        radioButtons('umap_color_by', 
                                     label = 'Color samples by',
                                     choices = 'Loading...',
                                     selected = character(0))),
                    div(style = 'padding-bottom: 20px;',
                        radioButtons('umap_shape_by', 
                                     label = 'Shape samples by',
                                     choices = 'Loading...',
                                     selected = character(0))
                    ),
                    div(style = 'padding-top: 80px; padding-left: 20px;',
                        downloadButton('download_umap_pdf', 'Download plot (PDF)',
                                   style = 'color: #ffffff; background-color: #42B5BB; border-color: #38a1a6;
    border-radius: 5px;')
                    )
                  )
              ),
              fluidRow(
                box(title = "Sample metadata",
                    div(style='padding-left: 20px; padding-bottom: 10px;', 
                        uiOutput('info_samples')),
                    width = 12,
                    div(style = 'padding-left: 20px; overflow-y: auto; height: 600px;',
                        withSpinner(dataTableOutput('table_sample_metadata'),
                                    type = 4, color = '#42B5BB'))
                )
              )
      ),
      
      tabItem(tabName = "tab_diff_expr",
              fluidRow(
                box(title = "Differential expression analysis parameters", 
                    width = 5,
                    height = 500,
                    div(style = 'padding-left: 20px;',
                        selectInput('select_volcano_column', 'Split groups by',
                                        'Loading...', selected = character(0)),
                        div(style = 'float: left;',
                            textInput('text_group1', 'Group A Name', 'Group A'),
                            selectInput('select_group1_criteria', 'Group A criteria',
                                        'Loading...', selected = character(0))
                            
                        ),
                        div(style = 'float: left; padding-left: 20px;',
                            textInput('text_group2', 
                                      label = 'Group B Name', 'Group B'),
                            selectInput('select_group2_criteria', 'Group B criteria',
                                        'Loading...', selected = character(0))
                        ),
                        div(style = 'padding-bottom: 20px;',
                            actionButton('button_run_volcano', 'Run analysis',
                                         icon = icon("angle-double-right"),
                                         style = 'color: #ffffff; background-color: #42B5BB; border-color: #38a1a6;
    border-radius: 5px;')),
                        div(downloadButton('download_volcano_pdf', 'Download plot (PDF)',
                                       style = 'color: #ffffff; background-color: #42B5BB; border-color: #38a1a6;
    border-radius: 5px;'))
                    )
                ),
                box(title = "Volcano plot",
                    width = 7,
                    height = 500,
                    div(withSpinner(plotlyOutput('volcano_plot'),
                                    type = 4, color = '#42B5BB'))
                )
              ),
              fluidRow(
                box(title = "Differentially expressed genes",
                    width = 12,
                    div(style = 'padding-left: 20px; overflow-y: auto; height: 600px;',
                        withSpinner(dataTableOutput('table_differential_expression'),
                                    type = 4, color = '#42B5BB'))
                )
              )
      ),
      
      tabItem(tabName = "tab_pathway_enrichment",
              h2("Plots tab content")
      ),
      
      tabItem(tabName = "tab_biomarkers",
              h2("Coming soon!")
      ),
      
      tabItem(tabName = "tab_druid",
              h2("Coming soon!")
      )
    )
  )
)