library(shiny)
library(httr)
library(rjson)
library(DT)
library(plotly)
options(repos = BiocManager::repositories())
library(limma)
library(DESeq2)

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

# ----------------------------------- Server --------------------------------- #

server <- function(input, output, session) {
  
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
  token_response <- httr::content(token_request, type = NULL)
  
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
  observeEvent(get_synapse_user_profile(), {
    query_string = paste0(getQueryString(), '&uid=', user_id)
    updateQueryString(query_string, mode = 'replace')
  })
  
  # Get the user's teams
  teams_response <- get_synapse_teams(user_id)
  teams = unlist(lapply(teams_response$results, function(l) paste0(l$name, ' (', l$id, ')')))
  team_ids = unlist(lapply(teams_response$results, function(l) paste0('team_', l$id)))
  teams_content_formatted = paste(teams, collapse = '\n')
  
  # Format team(s) and project(s) enabled for this app that the user can access
  enabled_teams = team_ids[team_ids %in% names(PROJECT_CONFIG)]
  PROJECT_DROPDOWN_LIST = list()
  ALL_ANALYSES = list()
  for (team_id in enabled_teams){
    projects = PROJECT_CONFIG[[team_id]]
    for (project in projects){
      PROJECT_DROPDOWN_LIST[project$project_name] = list(names(project$analyses))
      for (analysis in names(project$analyses)){
        ALL_ANALYSES[[analysis]] = c(PROJECT_CONFIG[[team_id]][[project$project_name]][['analyses']][[analysis]])
      }
    }
  }
  
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
      modalDialog(title = tagList(
                          img(src = 'www/synapse_logo.png', width = 200)
                  ),
                  h4(paste0(profile_response$firstName, ' ', profile_response$lastName)),
                  p(profile_response$company),
                  p(user_response$email, style = 'color: #27adde;'),
                  easyClose = T,
                  footer = tagList(
                    actionButton("button_view_syn_profile", "View Profile on Synapse",
                                 style = 'color: #ffffff; background-color:  #27adde; border-color: #1ea0cf;',
                                 onclick = paste0("window.open('https://www.synapse.org/#!Profile:", profile_response$ownerId, "', '_blank')")),
                    modalButton("Back to Analysis")
                    #actionButton("button_logout", "Log Out")
                  )
      )
    )
  })
  
  # Project info modal
  observeEvent(input$info_projects_modal, {
    showModal(modalDialog(
      title = 'Selecting a Synapse Project',
      p("The Projects listed in this dropdown menu are associated with your Synapse account. You must be granted access to a Project in Synapse in order to view it here. Note that some Projects may not be enabled for this app."),
      p("If you belong to multiple Teams on Synapse, the Projects you see in this menu will be grouped underneath the Team name."),
      p("Contact the Predictive BioAnalytics group (", 
        a('midas@wyss.harvard.edu', href='mailto:midas@wyss.harvard.edu',
          style = 'color: #27adde;'), 
        ") if you have any questions!"),
      easyClose = T,
      footer = NULL
    ))
  })
  
  output$logged_user <- renderText({
    if(logged_in()){
      return(paste0('Welcome, ', profile_response$firstName, '!'))
    }
  })
  
  # ---------------------- Methods writing functions ------------------- #
  
  generate_umap_methods <- function(n_genes, color_by, shape_by){
    in_file = 'data/methods/umap.txt'
    out_file = 'data/methods/umap_filled.txt'
    txt = read.table(in_file, sep = '|', stringsAsFactors = F)
    editable_txt = gsub('<GENE>', n_genes, txt[1,])
    editable_txt = gsub('<COLOR>', color_by, editable_txt)
    editable_txt = gsub('<SHAPE>', shape_by, editable_txt)
    txt[1,1] = editable_txt
    write.table(txt, out_file, sep = '\t', row.names = F, quote = F, col.names = F)
  }
  
  generate_volcano_methods <- function(a, numa, b, numb, method){
    in_file = 'data/methods/deg.txt'
    out_file = 'data/methods/deg_filled.txt'
    txt = read.table(in_file, sep = '|', stringsAsFactors = F)
    editable_txt = gsub('<A>', a, txt[1,])
    editable_txt = gsub('<NUMA>', numa, editable_txt)
    editable_txt = gsub('<B>', b, editable_txt)
    editable_txt = gsub('<NUMB>', numb, editable_txt)
    txt[1,1] = editable_txt
    write.table(txt, out_file, sep = '\t', row.names = F, quote = F, col.names = F)
  }
  
  
  # -------------------------- Tab 1: Samples -------------------------- #
  
  experimentData <- reactiveValues(selected_project = NULL,
                                   umap_color_by = NULL,
                                   umap_shape_by = NULL,
                                   sample_metadata_df = NULL,
                                   sample_color_columns = NULL,
                                   sample_shape_columns = NULL,
                                   gene_counts_df = NULL,
                                   umap_df = NULL,
                                   platform = NULL,
                                   data_loaded = F)
  
  # Load the projects the user has access to
  observeEvent(logged_in(), {
    updateSelectInput(session, 'project_select', 
                      choices = PROJECT_DROPDOWN_LIST)
    experimentData$selected_project <- PROJECT_DROPDOWN_LIST[[1]][1]
  })
  
  observeEvent(input$project_select, {
    if (input$project_select != 'Loading...'){
      experimentData$selected_project <- input$project_select
    }
  })
  
  selected_project <- reactive({ experimentData$selected_project })
  
  # Load the sample data, gene counts, and UMAP for the analysis
  observeEvent(selected_project(), {
    proj = selected_project()
    if (!is.null(proj)){
      ANALYSIS = ALL_ANALYSES[[proj]]
      sample_metadata_csv = fetch_synapse_filepath(ANALYSIS$metadata)
      experimentData$sample_metadata_df <- read.csv(sample_metadata_csv,
                                                    stringsAsFactors = F)
      gene_counts_csv = fetch_synapse_filepath(ANALYSIS$counts)
      experimentData$gene_counts_df <- read.csv(gene_counts_csv,
                                                stringsAsFactors = F)
      umap_csv = fetch_synapse_filepath(ANALYSIS$umap)
      experimentData$umap_df <- read.csv(umap_csv,
                                         row.names = 1,
                                         stringsAsFactors = F)
      experimentData$platform <- ANALYSIS$platform
      experimentData$data_loaded <- T
      
      # Extract colorable columns from column names
      dat = experimentData$sample_metadata_df
      experimentData$sample_color_columns <- sort(names(dat)[sapply(names(dat), function(x) {
        length(unique(dat[,x])) <= 11
      })])
      
      # Extract shapeable columns from column names
      experimentData$sample_shape_columns <- sort(names(dat)[sapply(names(dat), function(x) {
        length(unique(dat[,x])) <= 5
      })])
    }
  })
  
  sample_dat <- reactive({ experimentData$sample_metadata_df })
  counts_dat <- reactive({ experimentData$gene_counts_df })
  umap_dat <- reactive({ experimentData$umap_df })
  platform <- reactive({ experimentData$platform })
  loaded <- reactive({ experimentData$data_loaded })
  color_columns <- reactive({ experimentData$sample_color_columns })
  shape_columns <- reactive({ experimentData$sample_shape_columns })
  
  observeEvent(selected_project(), {
    proj = selected_project()
    if (!is.null(proj)){
      color_choices = color_columns()
      shape_choices = shape_columns()
      sample_metadata = sample_dat()
      updateRadioButtons(session, 'umap_color_by', 
                         choices = color_choices, selected = color_choices[1])
      updateRadioButtons(session, 'umap_shape_by', 
                         choices = shape_choices, selected = shape_choices[length(shape_choices)])
      updateSelectInput(session, 'select_volcano_column', 
                        choices = names(sample_metadata), selected = names(sample_metadata)[2])
    }
  })
  
  selected_color_column <- reactive({ input$umap_color_by })
  selected_shape_column <- reactive({ input$umap_shape_by })
  
  observeEvent(input$info_umap_modal, {
    showModal(
      modalDialog(title = "Visualizing sample similarity with UMAP",
                  p('This plot helps us visualize how similar or different samples are, based on their gene expression. 
                    Similar to a principal components analysis (PCA), the Uniform Manifold Approximation and 
                    Projection (UMAP) algorithm is used here for dimensionality reduction. Briefly, UMAP takes the 
                    expression profile (all genes measured in the experiment, typically 15,000 - 30,000 genes) for every sample 
                    and condenses that information down so that it can be displayed in 2 dimensions. The output is a 
                    plot that summarizes sample-to-sample similarity.'),
                  p("Let's look at the following example plot: "),
                  div(style = 'padding: 30px;',
                      img(src = 'www/umap_example.png', width = 500)),
                  p('A couple of insights can be gleaned from the plot above. First, we see that samples from Group 1 (
                    red), Group 2 (green), and Group 3 (blue) tend to cluster near other samples from the same group. This 
                    means that samples within each Group have similar expression profiles.'),
                  p('Secondly, we see that Group 1, 2, and 3 samples that have been treated with drug (triangle shape) 
                    still cluster by group (color), but all drug-treated samples cluster together too, showing that the 
                    drug treatment likely had a consistent effect on samples from all Groups.'),
                  p("Finally, there is one outlier green triangle point at the very bottom of the plot. Given the consistency 
                    of the other sample clustering, this sample appears to be a clear outlier. It's possible that 
                    the sample was mislabeled or that something went wrong during the RNA-seq/microarray measurement. When this happens,
                    it's worth performing some additional quality checks to investigate the cause."),
                  strong('References:'),
                  p("Leland McInnes, John Healy, James Melville. UMAP: Uniform Manifold Approximation and Projection for Dimension Reduction. 2018. ", 
                    a('Read the paper', href='https://arxiv.org/pdf/1802.03426.pdf',
                      target = '_blank', style = 'color: #27adde;'), 
                    " (opens in a new window)."),
                  easyClose = T,
                  footer = NULL)
      )
  })
  
  output$umap_plot <- renderPlotly({
    
    # User input plot params
    color_column = selected_color_column()
    shape_column = selected_shape_column()
    
    # Experimental data
    plot_mat = umap_dat()
    sample_metadata = sample_dat()
    
    if (loaded() & !is.null(color_column) & !is.null(shape_column)){
      
      if (color_column %in% names(sample_metadata) &
          shape_column %in% names(sample_metadata)){
      
        # Format dataframe for plotting
        row.names(sample_metadata) = sample_metadata$Sample_ID
        plot_df = cbind(sample_metadata[row.names(plot_mat),], plot_mat)
        names(plot_df)[(ncol(plot_df)-1):ncol(plot_df)] = c('V1', 'V2')
        
        # Make sure the color & shape columns are factors
        plot_df[,color_column] = as.factor(plot_df[,color_column])
        plot_df[,shape_column] = as.factor(plot_df[,shape_column])
        n_colors = length(unique(plot_df[,color_column]))
        n_symbols = length(unique(plot_df[,shape_column]))
        plot_cols = c('#27adde', sample(PLOT_COLORS[-10], n_colors-1))
        plot_symbols = sample(PLOT_SHAPES, n_symbols)
        
        # Plot
        p <- plot_ly(data = plot_df, x = ~V1, y = ~V2,
                     color = ~get(color_column), 
                     colors = plot_cols,
                     symbol = ~get(shape_column),
                     symbols = plot_symbols,
                     text = ~Sample_ID,
                     hovertemplate = '<b>Sample ID:</b> %{text}',
                     type = 'scatter', mode = 'markers',
                     marker = list(size = 10,
                                   line = list(
                                     color = '#212D32',
                                     width = 1
                                   )))
        
        # Save methods section
        generate_umap_methods(nrow(counts_dat()), color_column, shape_column)
        
        # Save to PDF
        pdf('data/sample_umap.pdf', height = 6, width = 8)
        x_max = max(plot_df$V1)
        x_min = min(plot_df$V1)
        x_range = x_max - x_min
        new_x_max = x_max + .5*x_range
        pdf_colors = plot_cols
        names(pdf_colors) = unique(plot_df[,color_column])
        pdf_symbols = plot_symbols
        names(pdf_symbols) = unique(plot_df[,shape_column])
        
        plot(plot_df$V1, plot_df$V2,
             main = 'Sample Similarity (UMAP algorithm)',
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
        plot_legend_colors = pdf_colors[as.character(sapply(unique_samples, function(x) strsplit(x, '__')[[1]][1]))]
        plot_legend_symbols = pdf_symbols[as.character(sapply(unique_samples, function(x) strsplit(x, '__')[[1]][2]))]
        legend('right', legend = plot_legend,
               pt.bg = plot_legend_colors,
               pch = plot_legend_symbols)
        dev.off()
        
        # Display Plotly plot in UI
        return(p)
        
      } else{
        return(NULL)
      }
    } else{
      return(NULL)
    } 
  })
  
  # Download UMAP plot as a PDF
  output$download_umap_pdf <- downloadHandler(
    filename = "Sample_similarity_UMAP.pdf",
    content = function(file) {
      file.copy("data/sample_umap.pdf", file)
    }
  )
  
  # Download UMAP methods as a .txt file
  output$download_umap_methods <- downloadHandler(
    filename = "Sample_similarity_UMAP_methods.txt",
    content = function(file) {
      file.copy('data/methods/umap_filled.txt', file)
    }
  )
  
  # Output the table of all sample metadata
  output$table_sample_metadata <- DT::renderDT({
    df = sample_dat()
    if (!is.null(df)){
      return(datatable(df, rownames = F, selection = 'none',
                       style = 'bootstrap'))
    } else{
      return(NULL)
    }
  })
  
  observeEvent(input$info_sample_metadata_modal, {
    showModal(
      modalDialog(title = "Sample metadata table",
                  p('This table contains annotations describing each sample used in the experiment. The data was 
                    uploaded to Synapse and can be modified on Synapse if corrections are needed. The color and shape 
                    parameters for the UMAP plot above are dynamically generated from the column names of this file, 
                    so column names can vary as needed by experiment as long as the sample ID is in the first column.'),
                  a('View the original file on Synapse', href = paste0('https://www.synapse.org/#!Synapse:', ANALYSIS$metadata),
                      target = '_blank', style = 'color: #27adde;'),
                  easyClose = T,
                  footer = NULL)
    )
  })
  
  # ------------------------- Tab 2: Diff Expr ------------------------- #
  
  diffExprData <- reactiveValues(message = NULL,
                                 group1_samples = NULL,
                                 group2_samples = NULL,
                                 diff_expr_result = NULL,
                                 diff_expr_csv = NULL,
                                 boxplot_df = NULL)
  
  volcano_column <- reactive({ input$select_volcano_column })
  group1_criteria <- reactive({ input$select_group1_criteria })
  group2_criteria <- reactive({ input$select_group2_criteria })
  group1 <- reactive({ diffExprData$group1_samples })
  group2 <- reactive({ diffExprData$group2_samples })
  
  most_recent_result <- reactive({ diffExprData$diff_expr_result })
  most_recent_boxplot <- reactive({ diffExprData$boxplot_df })
  
  observeEvent(input$info_preproc_modal, {
    showModal(
      modalDialog(title = paste0(selected_project(), " - gene expression data preprocessing"),
                  p('This table contains annotations describing each sample used in the experiment. The data was 
                    uploaded to Synapse and can be modified on Synapse if corrections are needed. The color and shape 
                    parameters for the UMAP plot above are dynamically generated from the column names of this file, 
                    so column names can vary as needed by experiment as long as the sample ID is in the first column.'),
                  easyClose = T,
                  footer = NULL)
    )
  })
  
  output$message_diff_expr <- renderUI({
    msg = 'The comparison is performed as "Group A vs Group B," so we recommend setting Group B as your control.'
    if (!is.null(diffExprData$message)){
      msg = diffExprData$message
    }
    return(helpText(msg))
  })
  
  observeEvent(selected_project(), {
    proj = selected_project()
    if (!is.null(proj)){
      dat = sample_dat()
      all_columns = names(dat)
      initial_column = all_columns[2]
      
      # Set volano column choices, and initial column selected
      updateSelectInput(session, 'select_volcano_column', 
                        choices = all_columns, selected = initial_column)
    }
  })
  
  observeEvent(volcano_column(), {
    
    selected_column = volcano_column()
    
    if (!is.null(selected_column) & selected_column != 'Loading...'){
      dat = sample_dat()
      
      # Allow the unique values in that column to be used as filter options
      filter_options = unique(as.character(dat[,selected_column]))
      updateSelectInput(session, 'select_group1_criteria', 
                        label = paste0('Group A ', selected_column, ' ='),
                        choices = filter_options, selected = filter_options[1])
      updateSelectInput(session, 'select_group2_criteria', 
                        label = paste0('Group B ', selected_column, ' ='),
                        choices = filter_options, selected = filter_options[length(filter_options)])
    }
  })
  
  observeEvent(group1_criteria(), {
    column = volcano_column()
    dat = sample_dat()
    group1_filter = group1_criteria()
    if (loaded() & column %in% names(dat)){
      diffExprData$group1_samples <- dat[dat[,column] == group1_filter, 'Sample_ID']
      if (length(diffExprData$group1_samples) < 2 | length(diffExprData$group2_samples) < 2){
        diffExprData$message <- 'Too few samples to perform analysis. Please select another group with at least 2 samples.'
      } else{
        diffExprData$message <- NULL
      }
    }
  })
  
  observeEvent(group2_criteria(), {
    column = volcano_column()
    dat = sample_dat()
    group2_filter = group2_criteria()
    if (loaded() & column %in% names(dat)){
      diffExprData$group2_samples <- dat[dat[,column] == group2_filter, 'Sample_ID']
      if (length(diffExprData$group1_samples) < 2 | length(diffExprData$group2_samples) < 2){
        diffExprData$message <- 'Too few samples to perform analysis. Please select another comparison with at least 2 samples per group.'
      } else{
        diffExprData$message <- NULL
      }
    }
  })
  
  output$group1_group2_selected <- renderText({
    a = group1()
    b = group2()
    group1_filter = group1_criteria()
    group2_filter = group2_criteria()
    str = '<A> (n = <NUMA> samples) vs <B> (n = <NUMB> samples)'
    str = gsub('<A>', group1_filter, str)
    str = gsub('<B>', group2_filter, str)
    str = gsub('<NUMA>', length(a), str)
    str = gsub('<NUMB>', length(b), str)
    return(str)
  })
  
  observeEvent(input$button_run_volcano, {
    
    withProgress(message = 'Running differential expression analysis', value = 0, {
      
      a = group1()
      b = group2()
      
      diff_expr_method = 'DESeq2'
      
      # Increment the progress bar, and update the detail text
      incProgress(0.2, detail = 'Formatting data')
      
      group1_filter = group1_criteria()
      group2_filter = group2_criteria()
      column = volcano_column()
      count_data = counts_dat()
      sample_metadata = sample_dat()
        
      # Add a _# to gene symbols that are duplicated
      count_data$Gene_Symbol = make.unique(as.character(count_data$Gene_Symbol), sep = "_")
      
      # Format for DESeg2
      row.names(sample_metadata) = sample_metadata$Sample_ID
      sample_metadata$Sample_ID = NULL
      row.names(count_data) = count_data$Gene_Symbol
      count_data$Gene_Symbol = NULL
      
      if (!is.null(count_data) & !is.null(sample_metadata)
          & !is.null(a) & !is.null(b) & !is.null(column)
          & !is.null(group1_filter) & !is.null(group2_filter)){
        
        out_filename = paste0('data/diff_expr_', group1_filter, '_vs_', group2_filter, '.csv')
        out_filename_counts = paste0('data/diff_expr_', group1_filter, '_vs_', group2_filter, '_counts.csv')
        diffExprData$diff_expr_csv <- out_filename
        out_rnk = paste0('data/diff_expr_', group1_filter, '_vs_', group2_filter, '.rnk')
        
        count_data_tmp = count_data[,c(a, b)]
        sample_data_tmp = sample_metadata[c(a, b),]
        
        if (length(a) > 1 & length(b) > 1){
          
          incProgress(0.2, detail = 'Setting up comparisons (<30 sec)')
          
          if (file.exists(out_filename)){
            deg_tab = read.csv(out_filename, stringsAsFactors = F)
            boxplot_tab = read.csv(out_filename_counts, stringsAsFactors = F)
            
          } else{
              incProgress(0.3, detail = 'Performing analysis (~1 min)')
            
              # If there's only one column, need to transform back into a 2D df
              if (is.null(dim(sample_data_tmp))){
                sample_data_tmp = data.frame(sample_data_tmp, stringsAsFactors = F)
                names(sample_data_tmp) = column
              }
            
              plat = platform()
              
              if (plat == 'rnaseq'){
            
                # DESeq2 METHOD
                dds <- DESeqDataSetFromMatrix(countData = count_data_tmp,
                                              colData = sample_data_tmp,
                                              design = as.formula(paste0('~ ', column)))
                dds <- DESeq(object = dds)
                
                cont = c(column, group1_filter, group2_filter)
                
                res = results(dds,
                              contrast = cont,
                              pAdjustMethod = "fdr",
                              cooksCutoff = FALSE)
                
                deg_tab = data.frame(res[,c('baseMean', 'log2FoldChange', 'padj')])
                deg_tab = cbind(Gene = row.names(deg_tab), deg_tab)
                deg_tab$Gene = as.character(deg_tab$Gene)
                names(deg_tab)[-1] = c('AvgExpr', 'log2FC', 'adjPVal')
                deg_tab = deg_tab[order(deg_tab$adjPVal, decreasing = F),]
                
                # Trigger new boxplot (with normalized counts)
                norm_counts <- counts(dds, normalized = TRUE)
                boxplot_tab = as.data.frame(cbind(sample_data_tmp[,column], t(norm_counts)))
                names(boxplot_tab)[1] = column
                
              } else if (plat == 'microarray'){
              
                # Limma
                model_matrix = model.matrix(~0 + as.factor(sample_data_tmp[,column]))
                colnames(model_matrix) = c(group1_filter, group2_filter)
                
                # Contrasts
                contrasts_cmd = paste0('makeContrasts(', group1_filter, 
                                       '-', group2_filter, ', levels = model_matrix)')
                contrasts = eval(parse(text=contrasts_cmd))
                
                # Differential expression
                fit = lmFit(count_data_tmp, model_matrix)
                fit2 = contrasts.fit(fit, contrasts = contrasts)
                fit2 = eBayes(fit2)
                deg_tab = topTable(fit2, number = 100000, adjust = 'fdr')
                deg_tab = data.frame(Gene = row.names(deg_tab), deg_tab)
                deg_tab$Gene = as.character(deg_tab$Gene)
                deg_tab = deg_tab[,c(1,3,2,6)]
                names(deg_tab)[-1] = c('AvgExpr', 'log2FC', 'adjPVal')
                deg_tab = deg_tab[order(deg_tab$adjPVal, decreasing = F),]
                
                # Trigger new boxplot (with normalized counts)
                boxplot_tab = as.data.frame(cbind(sample_data_tmp[,column], t(count_data_tmp)))
                names(boxplot_tab)[1] = column
                
              } else{
                cat(paste0('Unrecognized platform: ', plat))
              }
              
              # Save table to prevent re-creating it in the same session
              write.table(deg_tab, out_filename, sep = ',', row.names = F, quote = F)
              write.table(boxplot_tab, out_filename_counts, sep = ',', row.names = F, quote = F)
              
              # Save methods file
              generate_volcano_methods(group1_filter, length(a), 
                                       group2_filter, length(b),
                                       diff_expr_method)
              
          }
          incProgress(0.2, detail = 'Finalizing (<30 sec)')
          diffExprData$diff_expr_result <- deg_tab
          diffExprData$boxplot_df <- boxplot_tab
        }
      }
    })
  })
  
  observeEvent(input$info_volcano_modal, {
    showModal(
      modalDialog(title = "Visualizing differential gene expression with volcano plots",
                  p('A volcano plot summarizes both the magnitude of expression change (x axis) and the statistical 
                    significance (y axis). Each point represents a gene.'),
                  p("Let's look at the following example plot: "),
                  div(style = 'padding: 30px;',
                      img(src = 'www/volcano_example.png', width = 500)),
                  p('In the plot above, the genes represented by green points had significantly lower expression in 
                    the Experimental samples compared to the samples in the Control group. Likewise, the genes represented 
                    by red points had significantly higher expression in the Experimental group. A "significant" change 
                    means an adjusted p-value of < 0.05. Genes represented by grey points did not show a significant difference 
                    in expression in the Experimental samples compared to the Control.'),
                  strong('References:'),
                  p("Matthew E. Ritchie, Belinda Phipson, Di Wu, Yifang Hu, Charity W. Law, Wei Shi, Gordon K. Smyth. limma powers differential expression analyses for RNA-sequencing and microarray studies. Nucleic Acids Research. 2015.",
                    a('View the paper', href='https://academic.oup.com/nar/article/43/7/e47/2414268',
                      target = '_blank', style = 'color: #27adde;'), 
                    " (opens in a new window)."),
                  p("Leland McInnes, John Healy, James Melville. UMAP: Uniform Manifold Approximation and Projection for Dimension Reduction. 2018. ", 
                    a('View the limma R package vignette', href='https://bioconductor.org/packages/release/bioc/html/limma.html',
                      target = '_blank', style = 'color: #27adde;'), 
                    " (opens in a new window)."),
                  easyClose = T,
                  footer = NULL)
    )
  })
  
  output$volcano_message <- renderUI({
    if (is.null(diffExprData$diff_expr_result)){
      return(p(style='padding-left: 20px; color: #D2D6DD;', 
               'Use the section to the left to set analysis parameters and click "Run analysis" to view results'))
    } else{
      return(p(style='padding-left: 20px; color: #D2D6DD;',
               'Hover over a point to view the gene name'))
    }
  })
  
  output$volcano_plot <- renderPlotly({
    
    group1_filter = isolate(group1_criteria())
    group2_filter = isolate(group2_criteria())
    
    deg_results = most_recent_result()
    p_cutoff = 0.05
    upper_fc_cutoff = 1
    lower_fc_cutoff = -1
    
    if (!is.null(deg_results)){
      
      deg_results$log10pval = -log10(deg_results$adjPVal)
      deg_results$sig = ifelse(deg_results$adjPVal < p_cutoff & deg_results$log2FC > upper_fc_cutoff, 'increased',
                               ifelse(deg_results$adjPVal < p_cutoff & deg_results$log2FC < lower_fc_cutoff, 'decreased', 
                                      'not significant'))
      
      plot_cols = c(PLOT_COLORS[2], '#3aa4a9', '#C3C5C7')
      names(plot_cols) = c('increased', 'decreased', 'not significant')
      
      # Plot
      p <- plot_ly(data = deg_results, x = ~log2FC, y = ~log10pval,
                   color = ~sig, 
                   colors = plot_cols,
                   text = ~Gene,
                   hovertemplate = '<b>Gene:</b> %{text}',
                   type = 'scatter', mode = 'markers',
                   marker = list(size = 10,
                                 line = list(
                                   color = '#212D32',
                                   width = 1
                                 ))) %>%
        layout(title = paste0(group1_filter, ' vs ', group2_filter),
               xaxis = list(title = 'log2 fold change'),
               yaxis = list(title = '-log10 adjusted p-value')
        )
      
      # Save plot as PDF
      pdf('data/diffexpr_volcano.pdf', height = 8, width = 6)
      volcano_plot(deg_results$log2FC, deg_results$adjPVal, 
                   p_adj_threshold = p_cutoff, 
                   plot_main = paste0(group1_filter, ' vs ', group2_filter))
      legend('bottomright', bg = 'white',
             pch = 21, pt.bg = plot_cols,
             legend = names(plot_cols))
      dev.off()
      
      return(p)
    } else{
      return(NULL)
    }
  })
  
  # Download volcano plot as a PDF
  output$download_volcano_pdf <- downloadHandler(
    filename = "DEG_volcano.pdf",
    content = function(file) {
      file.copy("data/diffexpr_volcano.pdf", file)
    }
  )
  
  # Download differential expression analysis methods as a .txt file
  output$download_volcano_methods <- downloadHandler(
    filename = "DEG_methods.txt",
    content = function(file) {
      if (!is.null(diffExprData$diff_expr_result)){
        file.copy('data/methods/deg_filled.txt', file)
      } else{
        file.copy('data/methods/deg.txt', file)
      }
    }
  )
  
  output$table_differential_expression <- DT::renderDT({
    df = most_recent_result()
    
    if (!is.null(df)){
      
      # Replace gene names with hyperlink
      gene_names = df$Gene
      df$Gene = sapply(gene_names, function(s){
        HTML(paste0("<a href='https://www.genecards.org/cgi-bin/carddisp.pl?gene=", s, "' target='_blank'>", s,"</a>"))
      })
      df = df[,c('Gene', 'adjPVal', 'log2FC', 'AvgExpr')]
      
      # Add protein links
      df$Protein = sapply(gene_names, function(s){
        # Human Protein Atlas
        paste0(HTML(paste0("<a href='https://www.proteinatlas.org/search/", s, "' target='_blank'>", 'Human Protein Atlas',"</a>")), ' | ',
               HTML(paste0("<a href='https://www.uniprot.org/uniprot/?fil=organism%3A%22Homo+sapiens+%28Human%29+%5B9606%5D%22&sort=score&query=", s, "' target='_blank'>", 'UniProt',"</a>")))
      })
      
      # Round
      df$log2FC = round(as.numeric(df$log2FC), 3)
      df$AvgExpr = round(as.numeric(df$AvgExpr), 3)
      df$adjPVal = formatC(df$adjPVal, format = "e", digits = 3)
      
      # Display
      return(datatable(df, rownames = F, 
                       selection = 'single',
                       style = 'bootstrap', escape = F))
    } else{
      return(NULL)
    }
  })
  
  output$download_diff_expr_table <- downloadHandler(
    filename = function(){
      if (!is.null(diffExprData$diff_expr_csv)){
        gsub('data/', '', diffExprData$diff_expr_csv)
      }
    },
    content = function(file) {
      if (!is.null(diffExprData$diff_expr_csv)){
        file.copy(diffExprData$diff_expr_csv, file)
      }
    }
  )
  
  selected_gene <- reactive({ 
    row_selected = input$table_differential_expression_rows_selected
    df = most_recent_result()
    if (!is.null(row_selected)){
      return(df[row_selected,1])
    } else{
      # Show the top hit
      return(df$Gene[1])
    }
  })
  
  output$gene_boxplot <- renderPlotly({
    
    boxplot_df = most_recent_boxplot()
    boxplot_df$Sample_ID = row.names(boxplot_df)
    # To allow coloring of points
    group_column = names(boxplot_df)[1]
    boxplot_df$Color_ID = sapply(boxplot_df[,group_column], function(s) paste0('X', s))
    gene = selected_gene()
    boxplot_df[,gene] = log2(as.numeric(boxplot_df[,gene]))
    # TODO check these numbers
    #boxplot_df[,gene] = as.numeric(boxplot_df[,gene])
    
    boxplot_cols = c('#CCCCCC', '#CCCCCC', PLOT_COLORS[5], '#27adde')
    names(boxplot_cols) = c(sort(as.character(unique(boxplot_df[,group_column]))),
                            sort(unique(boxplot_df$Color_ID)))
      
    if (!is.null(boxplot_df)){
      
      p <- plot_ly(type = 'box', data = boxplot_df, 
                   x = ~get(names(boxplot_df)[1]), y = ~get(gene),
                   hoverinfo='none',
                   color = ~get(names(boxplot_df)[1]),
                   colors = boxplot_cols) %>%
           add_markers(~get(names(boxplot_df)[1]), y = ~get(gene),
                       type = 'scatter', mode = 'markers',
                       hoverinfo = 'text',
                       symbol = 21,
                       text = ~Sample_ID,
                       color = ~Color_ID,
                       marker = list(
                         size = 10,
                         line = list(
                           color = '#212D32',
                           width = 1
                       ))) %>%
           layout(showlegend = FALSE,
                  title = gene,
                  xaxis = list(title = 'Sample Group'),
                  yaxis = list(title = paste0(gene, ' (log2 normalized counts)')))
      
      # Save plot as PDF
      pdf(paste0('data/gene_boxplot_', gene, '.pdf'), 
          height = 6, width = 4)
      boxplot(get(gene)~get(group_column), data = boxplot_df,
              col = 'lightgrey', las = 1,
              main = gene, xlab = '',
              ylab = paste0(gene, ' (log2 normalized counts)'))
      points(get(gene)~get(group_column), data = boxplot_df,
             pch = 21, bg = boxplot_cols[boxplot_df$Color_ID])
      dev.off()
      
      return(p)
      
    } else{
      return(NULL)
    }
  })
  
  # Download gene boxplot plot as a PDF
  output$download_boxplot_pdf <- downloadHandler(
    filename = function(){
      paste0('gene_boxplot_', selected_gene(), '.pdf')
    },
    content = function(file) {
      file.copy(paste0('data/gene_boxplot_', selected_gene(), '.pdf'), file)
    }
  )
  
  observeEvent(input$info_diffexpr_modal, {
    showModal(
      modalDialog(title = "Differentially expressed gene table",
                  p("This table shows each gene and it's log fold change (logFC) calculated from comparing 
                    Group A - Group B according to the criteria you selected."),
                  strong('References:'),
                  p("Matthew E. Ritchie, Belinda Phipson, Di Wu, Yifang Hu, Charity W. Law, Wei Shi, Gordon K. Smyth. limma powers differential expression analyses for RNA-sequencing and microarray studies. Nucleic Acids Research. 2015.",
                    a('View the paper', href='https://academic.oup.com/nar/article/43/7/e47/2414268',
                      target = '_blank', style = 'color: #27adde;'), 
                    " (opens in a new window)."),
                  easyClose = T,
                  footer = NULL)
    )
  })
  
  output$row_diff_expr_results <- renderUI({
    boxplot_df = most_recent_boxplot()
    if (!is.null(boxplot_df)){
      return(tagList(
        box(title = tagList("Differentially expressed genes",
                            HTML('&nbsp;&nbsp;'),
                            tags$i(
                              class = "fa fa-info-circle", 
                              style = "color: #27adde; font-size: 8pt;"
                            ),
                            actionLink('info_diffexpr_modal', label = 'What is this?',
                                       style = 'font-size: 8pt; color: #27adde;')),
            width = 6,
            div(style = 'padding-left: 20px;',
                p(style='color: #D2D6DD;', "Click on a gene's row to display its boxplot to the right")),
            div(withSpinner(dataTableOutput('table_differential_expression'),
                            type = 4, color = '#27adde')),
            div(style = 'padding-left: 20px; padding-top: 52px; padding-bottom: 20px;',
                downloadButton('download_diff_expr_table', 'Download full table (.csv)',
                               style = 'color: #ffffff; background-color: #27adde; border-color: #1ea0cf;
            border-radius: 5px;')
            )
        ),
        box(title = "Box and whisker plot",
            width = 6,
            div(style = 'padding-left: 20px;',
                p(style='color: #D2D6DD;', "Hover over a point to see the sample name")),
            div(withSpinner(plotlyOutput('gene_boxplot'),
                            type = 4, color = '#27adde')),
            div(style = 'padding-left: 20px; padding-top: 65px; padding-bottom: 20px;',
                downloadButton('download_boxplot_pdf', 'Download boxplot (.pdf)',
                               style = 'color: #ffffff; background-color: #27adde; border-color: #1ea0cf;
            border-radius: 5px;')
            )
        ))
      )
    }
  })
  
}

# uiFunc instead of ui
shinyApp(uiFunc, server)
