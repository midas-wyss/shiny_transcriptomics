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
  
  # Ingber NIH Influenza Team
  team_3402263 = list(
    'NIH Influenza' = list(
      project_name = 'NIH Influenza',
      analyses = list(
        'Healthy vs COPD' = list(
          metadata = 'syn21645214',
          counts = 'syn21645216',
          umap = 'syn21645215',
          platform = 'rnaseq',
          notes = 'syn21771933'
        ),
        'Alveolus' = list(
          metadata = 'syn21641066',
          counts = 'syn21641065',
          umap = 'syn21641108',
          platform = 'rnaseq',
          notes = NULL
        )
      )
    ),
    'COPD + Smoking' = list(
      project_name = 'COPD + Smoking',
      analyses = list(
        'Benam 2017 chip - GSE87098' = list(
          metadata = 'syn21766074',
          counts = 'syn21766075',
          umap = 'syn21766080',
          platform = 'microarray',
          notes = NULL
        ),
        'Harvey 2007 clinical - GSE4498' = list(
          metadata = 'syn21766070',
          counts = 'syn21766071',
          umap = 'syn21766072',
          platform = 'microarray',
          notes = NULL
        )
      )
    )
  ),
  
  # Ingber Gates EED Team
  team_3406002 = list(
    'Gates Transcriptomics' = list(
      project_name = 'Gates Transcriptomics',
      analyses = list(
        'EED vs Healthy (with ND)' = list(
          metadata = 'syn21761311',
          counts = 'syn21761312',
          umap = 'syn21765942',
          platform = 'microarray',
          notes = NULL
        ),
        'AZM vs Control (with N/T def)' = list(
          metadata = 'syn21785559',
          counts = 'syn21785560',
          umap = 'syn21785561',
          platform = 'microarray',
          notes = NULL
        )
      )
    )
  ),
  
  # App Example User Team
  team_3406142 = list(
    'Example Project' = list(
      project_name = 'Example Project',
      analyses = list(
        'Example Analysis 1' = list(
          metadata = 'syn21645211',
          counts = 'syn21645213',
          umap = 'syn21645212',
          platform = 'rnaseq',
          notes = NULL
        ),
        'Example Analysis 2' = list(
          metadata = 'syn21645211',
          counts = 'syn21645213',
          umap = 'syn21645212',
          platform = 'rnaseq',
          notes = NULL
        )
      )
    )
  )
)
