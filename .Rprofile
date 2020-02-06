# ----------------------------- Environment --------------------------------- #
if (!Sys.info()[['sysname']] == 'Darwin'){
  # If running on shinyapps.io, set the RETICULATE_PYTHON evironment variable accordingly
  Sys.setenv(RETICULATE_PYTHON = '/home/shiny/.virtualenvs/python35_env/bin/python')
  # Set local debug to false
  Sys.setenv(DEBUG = FALSE)
  Sys.setenv(DISABLE_AUTH = FALSE)
} else{
  Sys.setenv(DEBUG = TRUE)
  Sys.setenv(DISABLE_AUTH = TRUE)
}
