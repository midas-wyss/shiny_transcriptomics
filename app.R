library(shiny)
library(httr)
library(rjson)
library(DT)
library(plotly)

DEBUG <- Sys.getenv('DEBUG') == 'TRUE'

# ----------------------------------- App UI --------------------------------- #

# Check whether user has auth'd
has_auth_code <- function(params) {
  # params is a list object containing the parsed URL parameters. Return TRUE if
  # based on these parameters, it looks like auth code is present that we can
  # use to get an access token. If not, it means we need to go through the OAuth
  # flow.
  return(!is.null(params$code))
}

# UI will change depending on whether the user has logged in
uiFunc <- function(req) {
  if (!has_auth_code(parseQueryString(req$QUERY_STRING))) {
    # Login button
    AnonymousUI
  } else {
    # App UI
    AuthenticatedUI
  }
}

# Import UI to be shown after user before and after auth'd
source('app_ui.R')
if (!dir.exists('data')){
  dir.create('data')
}

# ----------------------------------- Server --------------------------------- #

server <- function(input, output, session) {
  
  # ------------------------ Virtualenv setup -------------------------- #
  if (Sys.info()[['sysname']] != 'Darwin'){
    # When running on shinyapps.io, create a virtualenv 
    reticulate::virtualenv_create(envname = 'python35_txn_env', 
                                  python = '/usr/bin/python3')
    reticulate::virtualenv_install('python35_txn_env', 
                                   packages = c('synapseclient', 'requests'))
  }
  reticulate::use_virtualenv('python35_txn_env', required = T)
  reticulate::source_python('connect_to_synapse.py')
  
  # ---------------------------- OAuth --------------------------------- #
  # Initialize Synapse client
  login_to_synapse(username = Sys.getenv('SYN_USERNAME'),
                   api_key = Sys.getenv('SYN_API_KEY'))
  logged_in <- reactiveVal(FALSE)
  source('oauth.R')
  
  # Click on the 'Log in' button to kick off the OAuth round trip
  observeEvent(input$action, {
    session$sendCustomMessage("customredirect", oauth2.0_authorize_url(API, APP, scope = SCOPE))
    return()
  })
  
  params <- parseQueryString(isolate(session$clientData$url_search))
  if (!has_auth_code(params)) {
    return()
  }
  
  url <- paste0(API$access, '?', 'redirect_uri=', APP_URL, '&grant_type=', 
               'authorization_code', '&code=', params$code)
  
  # Get the access_token and userinfo token
  token_request <- POST(url,
                        encode = 'form',
                        body = '',
                        authenticate(APP$key, APP$secret, type = 'basic'),
                        config = list()
  )
  
  stop_for_status(token_request, task = 'Get an access token')
  token_response <- content(token_request, type = NULL)
  
  access_token <- token_response$access_token
  id_token <- token_response$id_token
  if (token_request$status_code == 201){
    logged_in(T)
  }
  
  # ------------------------------ App --------------------------------- #
  
  # Get information about the user
  user_response = get_synapse_userinfo(access_token)
  user_id = user_response$userid
  user_content_formatted = paste(lapply(names(user_response), 
                                        function(n) paste(n, user_response[n])), collapse="\n")
  
  # Get user profile
  profile_response <- get_synapse_user_profile()
  
  # Get the user's teams
  teams_response <- get_synapse_teams(user_id)
  teams = unlist(lapply(teams_response$results, function(l) paste0(l$name, ' (', l$id, ')')))
  team_ids = unlist(lapply(teams_response$results, function(l) paste0('team_', l$id)))
  teams_content_formatted = paste(teams, collapse = '\n')
  
  # Select team(s) that have project(s) enabled for this app
  enabled_teams = team_ids[team_ids %in% names(PROJECT_CONFIG)]
  # TEMP use the first team
  TEAM_ID = enabled_teams[1]
  
  # Get projects associated with that team - why is this empty?
  projects_response <- get_synapse_projects(access_token)
  
  # Cache responses
  if (DEBUG){
    saveRDS(token_response, 'cache/token_response.rds')
    saveRDS(user_response, 'cache/user_response.rds')
    saveRDS(teams_response, 'cache/teams_response.rds')
    saveRDS(projects_response, 'cache/projects_response.rds')
    saveRDS(profile_response, 'cache/profile_response.rds')
  }
  
  output$userInfo <- renderText(user_content_formatted)
  output$teamInfo <- renderText(teams_content_formatted)
  # See in app_ui.R with verbatimTextOutput("userInfo")
  
  # ---------------------------- Menus --------------------------------- #
  
  # Logout modal
  observeEvent(input$user_account_modal, {
    showModal(
      modalDialog(title = "Synapse Account Information",
                  h4(paste0(profile_response$firstName, ' ', profile_response$lastName)),
                  p(profile_response$company),
                  easyClose = T,
                  footer = tagList(
                    modalButton("Back to Analysis"),
                    actionButton("button_view_syn_profile", "View Profile on Synapse",
                                 onclick = paste0("window.open('https://www.synapse.org/#!Profile:", profile_response$ownerId, "', '_blank')"))
                    #actionButton("button_logout", "Log Out")
                  ))
    )
  })
  
  # Project info modal
  observeEvent(input$info_modal, {
    showModal(modalDialog(
      title = 'Selecting a Synapse Project',
      "The Projects listed in this dropdown menu are associated with your Synapse account. You must be granted access to a Project in Synapse in order to view it here. Note that some projects may not be enabled for this app. Contact Rani Powers / the Predictive BioAnalytics group (midas@wyss.harvard.edu) if you have any questions.",
      easyClose = T,
      footer = NULL
    ))
  })
  
  output$logged_user <- renderText({
    if(logged_in()){
      return(paste0('Welcome, ', profile_response$firstName, '!'))
    }
  })
  
  # -------------------------- Tab 1: Samples -------------------------- #
  
  experimentData <- reactiveValues(sample_metadata_df = NULL,
                                   sample_color_columns = NULL,
                                   sample_shape_columns = NULL,
                                   gene_counts_df = NULL,
                                   umap_df = NULL,
                                   data_loaded = F)
  
  # Load the sample data, gene counts, and UMAP for the project
  project_data = PROJECT_CONFIG[[TEAM_ID]]
  sample_metadata_csv = fetch_synapse_filepath(project_data$metadata)
  experimentData$sample_metadata_df <- read.csv(sample_metadata_csv,
                                                stringsAsFactors = F)
  gene_counts_csv = fetch_synapse_filepath(project_data$counts)
  experimentData$gene_counts_df <- read.csv(gene_counts_csv,
                                            row.names = 1,
                                            stringsAsFactors = F)
  umap_csv = fetch_synapse_filepath(project_data$umap)
  experimentData$umap_df <- read.csv(umap_csv,
                                     row.names = 1,
                                     stringsAsFactors = F)
  experimentData$data_loaded = T
  
  sample_dat <- reactive({ experimentData$sample_metadata_df })
  counts_dat <- reactive({ experimentData$gene_counts_df })
  umap_dat <- reactive({ experimentData$umap_df })
  loaded <- reactive({ experimentData$data_loaded })
  
  # Extract colorable columns from column names
  color_columns <- reactive({
    dat = sample_dat()
    if (!is.null(dat)){
      return(sort(names(dat)[sapply(names(dat), function(x) {
        length(unique(dat[,x])) <= 11
      })]))
    } else{
      return(NULL)
    }
  })
  
  # Extract shapeable columns from column names
  shape_columns <- reactive({
    dat = sample_dat()
    if (!is.null(dat)){
      return(sort(names(dat)[sapply(names(dat), function(x) {
        length(unique(dat[,x])) <= 5
      })]))
    } else{
      return(NULL)
    }
  })
  
  observeEvent(loaded(), {
    color_choices = color_columns()
    shape_choices = shape_columns()
    updateRadioButtons(session, 'umap_color_by', 
                       choices = color_choices, selected = color_choices[1])
    updateRadioButtons(session, 'umap_shape_by', 
                       choices = shape_choices, selected = shape_choices[1])
  })
  
  output$umap_plot <- renderPlotly({
    
    if (loaded()){
      
      # User input plot params
      color_column = input$umap_color_by
      shape_column = input$umap_shape_by
      
      # Format dataframe for plotting
      plot_mat = umap_dat()
      sample_metadata = sample_dat()
      row.names(sample_metadata) = sample_metadata$well_name
      plot_df = cbind(sample_metadata[row.names(plot_mat),], plot_mat)
      names(plot_df)[(ncol(plot_df)-1):ncol(plot_df)] = c('V1', 'V2')
      
      # Make sure the color & shape columns are factors
      plot_df[,color_column] = as.factor(plot_df[,color_column])
      plot_df[,shape_column] = as.factor(plot_df[,shape_column])
      plot_cols = sample(PLOT_COLORS, length(unique(plot_df[,color_column])))
      plot_symbols = sample(PLOT_SHAPES, length(unique(plot_df[,shape_column])))
      
      # Plot
      p <- plot_ly(data = plot_df, x = ~V1, y = ~V2,
              color = ~get(color_column), 
              colors = plot_cols,
              symbol = ~get(shape_column),
              symbols = plot_symbols,
              text = ~well_name,
              hovertemplate = '<b>Sample ID:</b> %{text}',
              type = 'scatter', mode = 'markers',
              marker = list(size = 10))
      
      # Save to PDF
      pdf('data/sample_umap.pdf', height = 6, width = 8)
      x_max = max(plot_df$V1)
      x_min = min(plot_df$V1)
      x_range = x_max - x_min
      new_x_max = x_max + .5*x_range
      pdf_colors = plot_cols
      names(pdf_colors) = unique(plot_df[,color_column])
      pdf_symbols = PLOT_SHAPES
      names(pdf_symbols) = unique(plot_df[,shape_column])
      
      plot(plot_df$V1, plot_df$V2,
           main = 'Sample UMAP',
           xlab = 'UMAP dimension 1', 
           ylab = 'UMAP dimension 2',
           xlim = c(x_min, new_x_max),
           pch = pdf_symbols[plot_df[,shape_column]],
           bg = pdf_colors[plot_df[,color_column]],
           las = 1)
      
      unique_colors = as.character(unique(plot_df[,color_column]))
      unique_symbols = as.character(unique(plot_df[,shape_column]))
      all_samples_combined = paste0(plot_df[,color_column], '__', plot_df[,shape_column])
      unique_samples = unique(all_samples_combined)
      
      plot_legend = titlify(unique_samples)
      plot_legend_colors = pdf_colors[as.character(sapply(unique_samples, function(x) strsplit(x, '-')[[1]][1]))]
      plot_legend_symbols = pdf_symbols[as.character(sapply(unique_samples, function(x) strsplit(x, '-')[[1]][2]))]
      legend('right', legend = plot_legend,
             pt.bg = plot_legend_colors,
             pch = plot_legend_symbols)
      dev.off()
      
      return(p)
      
    } 
  })
  
  # Download UMAP plot as a PDF
  output$download_umap_pdf <- downloadHandler(
    filename = "Sample_UMAP.pdf",
    content = function(file) {
      file.copy("data/sample_umap.pdf", file)
    }
  )
  
  # Output the table of all sample metadata
  output$sample_metadata <- DT::renderDT({
    return(experimentData$sample_metadata_df)
  })
  
  # ------------------------- Tab 2: Diff Expr ------------------------- #
  
  #contrast_groups = read.table()
  
}

# uiFunc instead of ui
shinyApp(uiFunc, server)
