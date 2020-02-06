library(shiny)
library(httr)
library(rjson)

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
    session$sendCustomMessage("mymessage", oauth2.0_authorize_url(API, APP, scope = SCOPE))
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
  
  # Display some information about the user to show successful login
  user_response = get_synapse_userinfo(access_token)
  user_id = user_response$userid #3395740
  user_content_formatted = paste(lapply(names(user_response), 
                                        function(n) paste(n, user_response[n])), collapse="\n")
  
  output$user_info <- renderText(user_content_formatted)
  
}

# uiFunc instead of ui
shinyApp(uiFunc, server)
