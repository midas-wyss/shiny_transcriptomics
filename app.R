library(shiny)
library(httr)
library(rjson)
library(umap)
library(DT)
library(plotly)

INGBER_ID <- Sys.getenv('INGBER_ID')
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

# ----------------------------------- Server --------------------------------- #

server <- function(input, output, session) {
  
  # ------------------------ Virtualenv setup -------------------------- #
  if (Sys.info()[['sysname']] != 'Darwin'){
    # When running on shinyapps.io, create a virtualenv 
    reticulate::virtualenv_create(envname = 'python35_env', 
                                  python = '/usr/bin/python3')
    reticulate::virtualenv_install('python35_env', 
                                   packages = c('synapseclient', 'requests'))
  }
  reticulate::use_virtualenv('python35_env', required = T)
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
  team_ids = unlist(lapply(teams_response$results, function(l) l$id))
  teams_content_formatted = paste(teams, collapse = '\n')
  
  # Selected team
  team_id = teams_response$results[[2]][['id']]
  
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
                  p(profile_response$firstName),
                  p(profile_response$lastName),
                  p(profile_response$company),
                  easyClose = T,
                  footer = tagList(
                    modalButton("Back to Analysis"),
                    actionButton("button_view_syn_profile", "View Profile on Synapse",
                                 onclick = paste0("window.open('https://www.synapse.org/#!Profile:", profile_response$ownerId, "', '_blank')")),
                    actionButton("button_logout", "Log Out",
                                 onclick = paste0("window.open('", APP_URL, "')"))
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
                                   gene_counts_df = NULL)
  
  sample_dat <- reactive({ experimentData$sample_metadata_df })
  counts_dat <- reactive({ experimentData$gene_counts_df })
  
  # Compute UMAP when experimental data is read
  umap_mat <- reactive({
    counts_df = counts_dat()
    if (!is.null(counts_df)){
      u = umap(t(counts_df))
      saveRDS(u$layout, 'cache/u_layout.rds')
      return(u$layout)
    }
  })
  
  # Extract color columns from column names
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
  
  # Extract shape columns from column names
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
  
  # If the person is a member of the Ingber Lab group,
  # read the sample data and gene counts
  if (INGBER_ID %in% team_ids){
    sample_metadata_csv = fetch_synapse_filepath('syn21519470')
    experimentData$sample_metadata_df <- read.csv(sample_metadata_csv,
                                                  stringsAsFactors = F)
    gene_counts_csv = fetch_synapse_filepath('syn21519469')
    experimentData$gene_counts_df <- read.csv(gene_counts_csv,
                                              row.names = 1,
                                              stringsAsFactors = F)
  }
  
  output$umap_plot <- renderPlotly({
    
    # User input plot params
    n = input$plot_n
    color_column = input$umap_color_by
    shape_column = input$umap_shape_by
    
    # Format dataframe for plotting
    plot_mat = umap_mat()
    sample_metadata = sample_dat()
    row.names(sample_metadata) = sample_metadata$well_name
    plot_df = cbind(sample_metadata[row.names(plot_mat),], plot_mat)
    names(plot_df)[(ncol(plot_df)-1):ncol(plot_df)] = c('V1', 'V2')
    
    # Make sure the color & shape columns are factors
    plot_df[,color_column] = as.factor(plot_df[,color_column])
    plot_df[,shape_column] = as.factor(plot_df[,shape_column])
    
    # Plot
    plot_ly(data = plot_df, x = ~V1, y = ~V2,
            color = ~get(color_column), 
            colors = sample(PLOT_COLORS, length(unique(plot_df[,color_column]))),
            symbol = ~get(shape_column),
            symbols = sample(PLOT_SHAPES, length(unique(plot_df[,shape_column]))),
            text = ~well_name,
            hovertemplate = '<b>Sample ID:</b> %{text}',
            type = 'scatter', mode = 'markers',
            marker = list(size = 10))
  })
  
  # Output the table of all sample metadata
  output$sample_metadata <- DT::renderDT({
    return(experimentData$sample_metadata_df)
  })
  
  # ------------------------- Tab 2: Diff Expr ------------------------- #
  
  #contrast_groups = read.table()
  
}

# uiFunc instead of ui
shinyApp(uiFunc, server)
