# -------------------------------- Settings --------------------------------- #
if (!Sys.info()[['sysname']] == 'Darwin'){
  # If running on shinyapps.io, set the RETICULATE_PYTHON evironment variable
  Sys.setenv(RETICULATE_PYTHON = '/home/shiny/.virtualenvs/python37_txn_env/bin/python')
  # Set local debug to false
  Sys.setenv(DEBUG = FALSE)
} else{
  # Running locally, use the local virtualenv
  options(shiny.port = 7450)
  reticulate::use_virtualenv('python37_txn_env', required = T)
  Sys.setenv(DEBUG = TRUE)
}

# -------------------------------- Projects --------------------------------- #
# Note: these are Synapse accession numbers and are *not* secrets. 
# Don't put secrets here - store them as environment variables instead!

PROJECT_CONFIG <- list(
  # Ingber NIH Influenza Team
  team_3402263 = list(
    project_name = 'NIH Influenza',
    analyses = list(
      'Healthy vs COPD' = list(
        metadata = 'syn21645214',
        counts = 'syn21645216',
        umap = 'syn21645215'
      ),
      'Alveolus' = list(
        metadata = 'syn21641066',
        counts = 'syn21641065',
        umap = 'syn21641108'
      )
    )
  ),
  # App Example User Team
  team_3406142 = list(
    project_name = 'Example Project',
    analyses = list(
      'Example Analysis' = list(
        metadata = 'syn21645211',
        counts = 'syn21645213',
        umap = 'syn21645212'
      )
    )
  )
)
