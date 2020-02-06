library(shiny)

# Add www subdirectory for images and stuff
addResourcePath(prefix = 'www', directoryPath = paste0(getwd(), '/www'))

# We'll do a dynamic redirect after authenticating
# https://stackoverflow.com/questions/57755830/how-to-redirect-to-a-dynamic-url-in-shiny
jscode <- "Shiny.addCustomMessageHandler('mymessage', function(message) { window.location = message;});"

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

AuthenticatedUI <- fluidPage(
  theme = 'bootstrap.yeti.css',
  title = 'Shiny Synapse Auth Example App',
  
  # Logo
  div(img(src = paste0('www/logo.png'), width = '300px'),
      style='text-align: center;'),
  br(),
  
  # Success message
  h5('You successfully logged in!'),
  verbatimTextOutput('user_info')
  
)