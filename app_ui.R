library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(RColorBrewer)

# Add www subdirectory for images and stuff
addResourcePath(prefix = 'www', directoryPath = paste0(getwd(), '/www'))

# We'll do a dynamic redirect after authenticating
# https://stackoverflow.com/questions/57755830/how-to-redirect-to-a-dynamic-url-in-shiny
jscode <- "Shiny.addCustomMessageHandler('mymessage', function(message) { window.location = message;});"

# Plot color palettes
PLOT_COLORS <- brewer.pal(11, 'Spectral')
PLOT_SHAPES <- 21:25

# ------------------------------- Pre-login UI ------------------------------ #

AnonymousUI <- fluidPage(
  theme = 'bootstrap.yeti.css',
  title = 'Shiny Synapse Auth Example App',
  
  # Logo
  div(img(src = paste0('www/logo.png'), width = '300px'),
      style='text-align: center;'),
  br(),
  
  # Login button
  tags$head(tags$script(jscode)),
  div(actionButton("action", "Log in with Synapse"),
      style='text-align: center;'),
  br(),
  
  div("Don't have a Synapse account yet? Follow the ",
      a(href = 'https://midas-wyss.github.io/synapse_instructions_ingber.html',
        'instructions for registering here'),
      '.', style='text-align: center;')
)

# ------------------------------ Post-login UI ------------------------------ #

AuthenticatedUI <- dashboardPage(
  skin = 'blue',
  
  dashboardHeader(title = "Transcriptomics Explorer",
                  titleWidth = 250,
                  tags$li(class = "dropdown",
                          tags$li(class = "dropdown", 
                                  textOutput("logged_user"), 
                                  style = "padding: 15px 15px; color: #fff;"))),
  
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
    div(actionLink('info', 'What are Projects?', 
                   style = 'color: #42B5BB;'), 
        style = 'font-size: 8pt; margin: 0px 5px 20px 0px;'),
    sidebarMenu(
      menuItem("Samples", tabName = "samples", icon = icon("vial")),
      menuItem("Differential expression", tabName = "diff_expr", icon = icon("chart-bar")),
      menuItem("Biomarkers", tabName = "biomarkers", icon = icon("share-alt")),
      menuItem("DRUID", tabName = "druid", icon = icon("star"))
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
    tabItems(
      tabItem(tabName = "samples",
              fluidRow(
                box(title = "Between-sample variability (UMAP algorithm)", 
                    height = 500,
                    div(withSpinner(plotlyOutput('umap_plot'),
                                    type = 8, color = '#42B5BB'),
                        style = "overflow-y: auto;")),
                box(title = "Plot parameters",
                    height = 500,
                    radioButtons('umap_color_by', 
                                 label = 'Color samples by',
                                 choices = c('condition', 'group', 'time', 'group_time'),
                                 selected = 'group_time'),
                    radioButtons('umap_shape_by', 
                                 label = 'Shape samples by',
                                 choices = c('condition', 'group', 'time', 'group_time'),
                                 selected = 'condition')
                )
              ),
              fluidRow(
                box(title = "All sample meta-data",
                    width = 12,
                    div(withSpinner(dataTableOutput('sample_metadata'),
                                    type = 8, color = '#42B5BB'),
                        style = "overflow-y: auto; height: 600px;")
                )
              )
      ),
      
      tabItem(tabName = "diff_expr",
              h2("Plots tab content")
      )
    )
  )
)