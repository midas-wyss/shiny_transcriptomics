library(shiny)
library(shinydashboard)
library(shinycssloaders)

# Add www subdirectory for images and stuff
addResourcePath(prefix = 'www', directoryPath = paste0(getwd(), '/www'))

# We'll do a dynamic redirect after authenticating
# https://stackoverflow.com/questions/57755830/how-to-redirect-to-a-dynamic-url-in-shiny
jscode <- "Shiny.addCustomMessageHandler('customredirect', function(message) { window.location = message;});"

drift_js <- '<script>
"use strict";

!function() {
  var t = window.driftt = window.drift = window.driftt || [];
  if (!t.init) {
    if (t.invoked) return void (window.console && console.error && console.error("Drift snippet included twice."));
    t.invoked = !0, t.methods = [ "identify", "config", "track", "reset", "debug", "show", "ping", "page", "hide", "off", "on" ], 
    t.factory = function(e) {
      return function() {
        var n = Array.prototype.slice.call(arguments);
        return n.unshift(e), t.push(n), t;
      };
    }, t.methods.forEach(function(e) {
      t[e] = t.factory(e);
    }), t.load = function(t) {
      var e = 3e5, n = Math.ceil(new Date() / e) * e, o = document.createElement("script");
      o.type = "text/javascript", o.async = !0, o.crossorigin = "anonymous", o.src = "https://js.driftt.com/include/" + n + "/" + t + ".js";
      var i = document.getElementsByTagName("script")[0];
      i.parentNode.insertBefore(o, i);
    };
  }
}();
drift.SNIPPET_VERSION = "0.3.1";
drift.load("579ukhxa5466");
</script>'

source('plot_helpers.R')

# ------------------------------- Pre-login UI ------------------------------ #

AnonymousUI <- fluidPage(
  tags$head(tags$link(rel="stylesheet", type="text/css", href="bootstrap_rani.css"),
            HTML('<link rel="icon" href="www/favicon.ico" type="image/x-icon"/>')),
  title = 'Transcriptomics Explorer',
  
  # Wyss logo
  div(img(src = paste0('www/wyss-logo-white.png'), width = '200px'),
      style='text-align: center; padding-top: 30px;'),
  
  h1('Transcriptomics Explorer'),
  
  # DNA
  div(style='position: relative; height: 300px; margin-top: -80px;',
    HTML('<div class="dna" style="--strands: 16;">
      <div class="strand" style="--s: 0;"></div>
      <div class="strand" style="--s: 1;"></div>
      <div class="strand" style="--s: 2;"></div>
      <div class="strand" style="--s: 3;"></div>
      <div class="strand" style="--s: 4;"></div>
      <div class="strand" style="--s: 5;"></div>
      <div class="strand" style="--s: 6;"></div>
      <div class="strand" style="--s: 7;"></div>
      <div class="strand" style="--s: 8;"></div>
      <div class="strand" style="--s: 9;"></div>
      <div class="strand" style="--s: 10;"></div>
      <div class="strand" style="--s: 11;"></div>
      <div class="strand" style="--s: 12;"></div>
      <div class="strand" style="--s: 13;"></div>
      <div class="strand" style="--s: 14;"></div>
      <div class="strand" style="--s: 15;"></div>
      </div>')
  ),
  
  # Login button
  tags$head(tags$script(jscode),
            HTML(drift_js)),
  div(actionButton("action", "Log in with Synapse"),
      style='text-align: center; padding-bottom: 100px;'),
  
  div(p("Don't have a Synapse account yet?", style='text-align: center; font-size: 10pt;'),
      p("Follow the ",
        a(href = 'https://midas-wyss.github.io/synapse_instructions_predictive_bioanalytics.html',
          target = '_blank', 'instructions for registering here', style='font-size: 10pt;'), 
        '.', style='text-align: center; font-size: 10pt;'))
)

# ------------------------------ Post-login UI ------------------------------ #

AuthenticatedUI <- dashboardPage(
  skin = 'blue',
  
  dashboardHeader(title = 'Transcriptomics Explorer',
                  titleWidth = 250,
                  tags$li(class = "dropdown",
                          tags$li(class = "dropdown", 
                                  actionLink("user_account_modal", textOutput("logged_user")),
                                  style = "color: #fff;"))
  ),
  
  dashboardSidebar(
    width = 250,
    tags$head(tags$style(HTML('.logo {
                              background-color: #1ea0cf !important;
                              }
                              .navbar {
                              background-color: #27adde !important;
                              }
                              .sidebar-toggle {
                              background-color: #27adde !important;
                              }
                              .skin-blue .sidebar-menu > li.active > a,
                              .skin-blue .sidebar-menu > li:hover > a { border-left-color: #1ea0cf;}
                              .table.dataTable tbody tr.active td {
                                  background-color: #27adde !important;
                                  color: white;
                              }
                              .table.dataTable tbody tr.active td a {
                                  color: white;
                              }'
    ))),
    selectInput('project_select', 'Project',
                list('NIH Influenza' = c('Healthy vs COPD', 'Alveolar'))),
    div(actionLink('info_projects_modal', 'What are Projects?', 
                   style = 'color: #27adde;'), 
        style = 'font-size: 8pt; margin: 0px 5px 20px 0px;'),
    sidebarMenu(
      menuItem("Samples overview", tabName = "tab_samples", icon = icon("vial")),
      menuItem("Differential expression", tabName = "tab_diff_expr", icon = icon("chart-bar"))
      #menuItem("Pathway enrichment", tabName = "tab_pathway_enrichment", icon = icon("align-left")),
      #menuItem("Biomarkers analysis", tabName = "tab_biomarkers", icon = icon("share-alt")),
      #menuItem("DRUID", tabName = "tab_druid", icon = icon("star"))
    ),
    br(),
    div(style = 'position: fixed; bottom: 20px; margin: 0px 20px 0 20px; width: 180px;',
        img(src = 'www/wyss-logo-white-square.png', width = '140px'),
        p('This tool was developed by the Predictive BioAnalytics group',
          style = 'font-size: 8pt; margin-top: 10px',),
        a('View our other apps', href = 'https://midas-wyss.github.io/', 
          target = '_blank', style = 'font-size: 8pt; color: #27adde;')
    )
  ),
  
  dashboardBody(
    tags$head(includeHTML('www/analytics.html'),
              HTML(drift_js),
              tags$style(".shiny-output-error{color: white;}"),
              HTML('<link rel="icon" href="www/favicon.ico" type="image/x-icon"/>')),
    tabItems(
      tabItem(tabName = "tab_samples",
              h2('Samples overview'),
              fluidRow(
                box(title = tagList("Between-sample variability (UMAP algorithm)",
                                    HTML('&nbsp;&nbsp;'),
                                    tags$i(
                                      class = "fa fa-info-circle", 
                                      style = "color: #27adde; font-size: 8pt;"
                                    ),
                                    actionLink('info_umap_modal', label = 'What is this?',
                                               style = 'font-size: 8pt; color: #27adde;')), 
                    width = 7,
                    height = 500,
                    div(style = 'padding-left: 20px;',
                      p(style='color: #D2D6DD;', 'Hover over a point to view the sample name')
                    ),
                    div(withSpinner(plotlyOutput('umap_plot'),
                                    type = 4, color = '#27adde'),
                        style = "overflow-y: auto;")),
                box(title = "UMAP plot settings",
                    width = 5,
                    height = 500,
                    div(style = 'padding: 0 30px 20px 30px; float: left;',
                        radioButtons('umap_color_by', 
                                       label = 'Color samples by',
                                       choices = 'Loading...',
                                       selected = character(0))),
                    div(style = 'float: left;',
                          radioButtons('umap_shape_by', 
                                       label = 'Shape samples by',
                                       choices = 'Loading...',
                                       selected = character(0))
                    ),
                    div(style = 'padding-top: 50px; padding-left: 20px; clear: left;',
                        downloadButton('download_umap_pdf', 'Download plot (.pdf)',
                                   style = 'color: #ffffff; background-color: #27adde; border-color: #1ea0cf;
    border-radius: 5px;')
                    ),
                    div(style = 'padding-top: 20px; padding-left: 20px; clear: left;',
                        downloadButton('download_umap_methods', 'Download figure legend (.txt)',
                                       style = 'color: #ffffff; background-color: #27adde; border-color: #1ea0cf;
    border-radius: 5px;')
                    )
                  )
              ),
              fluidRow(
                box(title = tagList("Sample metadata",
                                    HTML('&nbsp;&nbsp;'),
                                    tags$i(
                                      class = "fa fa-info-circle", 
                                      style = "color: #27adde; font-size: 8pt;"
                                    ),
                                    actionLink('info_sample_metadata_modal', label = 'What is this?',
                                               style = 'font-size: 8pt; color: #27adde;')),
                    div(style='padding-left: 20px; padding-bottom: 10px;', 
                        uiOutput('info_samples')),
                    width = 12,
                    div(style = 'padding-left: 20px; overflow-y: auto;',
                        withSpinner(dataTableOutput('table_sample_metadata'),
                                    type = 4, color = '#27adde')),
                    div(style = 'padding-left: 20px; padding-bottom: 40px;',
                        downloadButton('download_metadata_table', 'Download full table (.csv)',
                                       style = 'color: #ffffff; background-color: #27adde; border-color: #1ea0cf;
        border-radius: 5px;')
                    )
                )
              )
      ),
      
      tabItem(tabName = "tab_diff_expr",
              h2('Differential Expression'),
              fluidRow(
                box(title = "Set parameters for analysis",
                    div(style = 'padding-left: 20px;',
                        p(style='color: #D2D6DD;', 'Select the criteria for comparing two sample groups')
                    ),
                    width = 6,
                    height = 500,
                    div(style='padding-left: 20px;',
                        div(style='width: 180px;',
                            selectInput('select_volcano_column', 'Split groups by',
                                        'Loading...', selected = character(0))),
                        div(style='float: left; width: 180px;',
                            #textInput('text_group1', 'Group A Name', 'Group A'),
                            selectInput('select_group1_criteria', 'Group A criteria',
                                        'Loading...', selected = character(0))
                        ),
                        div(style='float: left; width: 240px; padding-left: 50px;',
                            #textInput('text_group2', 'Group B Name', 'Group B'),
                            selectInput('select_group2_criteria', 'Group B criteria',
                                        'Loading...', selected = character(0))
                        ),
                    ),
                    div(style = 'padding-left: 20px; width: 440px;',
                        verbatimTextOutput('group1_group2_selected', F),
                        div(style = 'color: #D2D6DD;',
                          helpText('The comparison is performed as "Group A vs Group B," so we recommend setting Group B as your control.')
                        )
                    ),
                    div(style = 'padding-top: 40px; padding-left: 20px; clear: left;',
                        actionButton('button_run_volcano', 'Run analysis',
                                     icon = icon("angle-double-right"),
                                     style = 'color: #ffffff; background-color: #27adde; border-color: #1ea0cf; border-radius: 5px;')
                    ),
                    div(style = 'padding-top: 20px; padding-left: 20px; clear: left;',
                        div(style = 'float: left; padding-right: 20px;',
                          downloadButton('download_volcano_pdf', 'Download volcano plot (.pdf)',
                            style = 'color: #ffffff; background-color: #27adde; border-color: #1ea0cf; border-radius: 5px;')
                        ),
                        div(style = 'float: left;',
                          downloadButton('download_volcano_methods', 'Download methods (.txt)',
                                         style = 'color: #ffffff; background-color: #27adde; border-color: #1ea0cf; border-radius: 5px;')
                        )
                    )
                ),
                box(title = tagList("Volcano plot of differentially expressed genes",
                                    HTML('&nbsp;&nbsp;'),
                                    tags$i(
                                      class = "fa fa-info-circle", 
                                      style = "color: #27adde; font-size: 8pt;"
                                    ),
                                    actionLink('info_volcano_modal', label = 'What is this?',
                                               style = 'font-size: 8pt; color: #27adde;')),
                    width = 6,
                    height = 500,
                    uiOutput('volcano_message'),
                    div(withSpinner(plotlyOutput('volcano_plot'),
                                    type = 4, color = '#27adde'))
                )
              ),
              fluidRow(
                uiOutput('row_diff_expr_results')
             )
      )
      
      #tabItem(tabName = "tab_pathway_enrichment",
      #        h2("Pathway enrichment"),
      #        h5('Coming soon!')
      #),
      
      #tabItem(tabName = "tab_biomarkers",
      #        h2("Biomarker Identification"),
      #        h5('Coming soon!')
      #),
      
      #tabItem(tabName = "tab_druid",
      #        h2("DRUID"),
      #        h5('Coming soon!')
      #)
    )
  )
)