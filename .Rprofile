# -------------------------------- Settings --------------------------------- #
if (!Sys.info()[['sysname']] == 'Darwin'){
  # If running on shinyapps.io, set the RETICULATE_PYTHON evironment variable
  Sys.setenv(RETICULATE_PYTHON = '/home/shiny/.virtualenvs/python35_txn_env/bin/python')
  # Set local debug to false
  Sys.setenv(DEBUG = FALSE)
} else{
  # Running locally, use the local virtualenv
  options(shiny.port = 7450)
  reticulate::use_virtualenv('python35_txn_env', required = T)
  Sys.setenv(DEBUG = TRUE)
}

# -------------------------------- Projects --------------------------------- #
# Note: these are Synapse accession numbers and are *not* secrets. 
# Don't put secrets here - store them as environment variables instead!

PROJECT_CONFIG <- list(
  # Ingber lab
  team_3402263 = list(
    metadata = 'syn21519470',
    counts = 'syn21519468',
    umap = 'syn21584812'
  )
)
