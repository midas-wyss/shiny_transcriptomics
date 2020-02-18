library(shiny)
library(httr)
library(rjson)
library(DT)
library(plotly)
options(repos = BiocManager::repositories())
library(limma)

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
  observeEvent(input$info_modal, {
    showModal(modalDialog(
      title = 'Selecting a Synapse Project',
      p("The Projects listed in this dropdown menu are associated with your Synapse account. You must be granted access to a Project in Synapse in order to view it here. Note that some projects may not be enabled for this app."),
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
  
  generate_volcano_methods <- function(a, numa, b, numb){
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
  
  experimentData <- reactiveValues(sample_metadata_df = NULL,
                                   sample_color_columns = NULL,
                                   sample_shape_columns = NULL,
                                   gene_counts_df = NULL,
                                   umap_df = NULL,
                                   data_loaded = F)
  
  # Load the projects the user has access to
  project_data = PROJECT_CONFIG[[TEAM_ID]]
  analyses = project_data$analyses
  ANALYSIS = analyses[[1]]
  
  # Load the sample data, gene counts, and UMAP for the analysis 
  sample_metadata_csv = fetch_synapse_filepath(ANALYSIS$metadata)
  experimentData$sample_metadata_df <- read.csv(sample_metadata_csv,
                                                stringsAsFactors = F)
  gene_counts_csv = fetch_synapse_filepath(ANALYSIS$counts)
  experimentData$gene_counts_df <- read.csv(gene_counts_csv,
                                            row.names = 1,
                                            stringsAsFactors = F)
  umap_csv = fetch_synapse_filepath(ANALYSIS$umap)
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
                       choices = shape_choices, selected = shape_choices[2])
    updateSelectInput(session, 'select_volcano_column', 
                      choices = color_choices, selected = color_choices[1])
  })
  
  selected_color_column <- reactive({ input$umap_color_by })
  selected_shape_column <- reactive({ input$umap_shape_by })
  
  output$umap_plot <- renderPlotly({
    
    # User input plot params
    color_column = selected_color_column()
    shape_column = selected_shape_column()
    
    # Experimental data
    plot_mat = umap_dat()
    sample_metadata = sample_dat()
    
    if (loaded() & !is.null(color_column)){
      # Format dataframe for plotting
      row.names(sample_metadata) = sample_metadata$well_name
      plot_df = cbind(sample_metadata[row.names(plot_mat),], plot_mat)
      names(plot_df)[(ncol(plot_df)-1):ncol(plot_df)] = c('V1', 'V2')
      
      # Make sure the color & shape columns are factors
      plot_df[,color_column] = as.factor(plot_df[,color_column])
      plot_df[,shape_column] = as.factor(plot_df[,shape_column])
      n_colors = length(unique(plot_df[,color_column]))
      n_symbols = length(unique(plot_df[,shape_column]))
      plot_cols = c('#27adde', sample(PLOT_COLORS, n_colors-1))
      plot_symbols = sample(PLOT_SHAPES, n_symbols)  
      
      # Plot
      p <- plot_ly(data = plot_df, x = ~V1, y = ~V2,
                   color = ~get(color_column), 
                   colors = plot_cols,
                   symbol = ~get(shape_column),
                   symbols = plot_symbols,
                   text = ~well_name,
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
  })
  
  # Download UMAP plot as a PDF
  output$download_umap_pdf <- downloadHandler(
    filename = "Sample_UMAP.pdf",
    content = function(file) {
      file.copy("data/sample_umap.pdf", file)
    }
  )
  
  # Download UMAP methods as a .txt file
  output$download_umap_methods <- downloadHandler(
    filename = "UMAP_methods.txt",
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
  
  # Link to file on Synapse
  output$info_samples <- renderUI({
    actionLink('view_synapse_metadata', 'View original file on Synapse', 
             style = 'color: #27adde;',
             onclick = paste0("window.open('https://www.synapse.org/#!Synapse:", 
                              ANALYSIS$metadata, "', '_blank')"))
    })
  
  # ------------------------- Tab 2: Diff Expr ------------------------- #
  
  diffExprData <- reactiveValues(group1_samples = NULL,
                                 group2_samples = NULL,
                                 diff_expr_result = NULL)
  
  volcano_column <- reactive({ input$select_volcano_column })
  group1_criteria <- reactive({ input$select_group1_criteria })
  group2_criteria <- reactive({ input$select_group2_criteria })
  group1 <- reactive({ diffExprData$group1_samples })
  group2 <- reactive({ diffExprData$group2_samples })
  
  most_recent_result <- reactive({ diffExprData$diff_expr_result })
  
  observeEvent(volcano_column(), {
    column = volcano_column()
    dat = sample_dat()
    if (loaded() & column %in% names(dat)){
      filter_options = unique(as.character(dat[,column]))
      updateSelectInput(session, 'select_group1_criteria', 
                        label = paste0('Group A ', column, ' ='),
                        choices = filter_options, selected = filter_options[1])
      updateSelectInput(session, 'select_group2_criteria', 
                        label = paste0('Group B ', column, ' ='),
                        choices = filter_options, selected = filter_options[2])
    }
  })
  
  observeEvent(group1_criteria(), {
    column = volcano_column()
    dat = sample_dat()
    group1_filter = group1_criteria()
    if (loaded() & column %in% names(dat)){
      diffExprData$group1_samples <- dat[dat[,column] == group1_filter, 'well_name']
    }
  })
  
  observeEvent(group2_criteria(), {
    column = volcano_column()
    dat = sample_dat()
    group2_filter = group2_criteria()
    if (loaded() & column %in% names(dat)){
      diffExprData$group2_samples <- dat[dat[,column] == group2_filter, 'well_name']
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
    a = group1()
    b = group2()
    group1_filter = group1_criteria()
    group2_filter = group2_criteria()
    column = volcano_column()
    count_data = counts_dat()
    sample_data = sample_dat()
    
    # Format for DESeq
    count_data$entrez_id = NULL
    row.names(sample_data) = sample_data$well_name
    sample_data$well_name = NULL
    
    if (!is.null(count_data) & !is.null(sample_data)
        & !is.null(a) & !is.null(b) & !is.null(column)
        & !is.null(group1_filter) & !is.null(group2_filter)){
      
      out_filename = paste0('data/diff_expr_', column, '_', group1_filter, '_vs_', group2_filter, '.csv')
      out_rnk = paste0('data/diff_expr_', column, '_', group1_filter, '_vs_', group2_filter, '.rnk')
      
      if (file.exists(out_filename)){
        deg_tab = read.csv(out_filename, stringsAsFactors = F)
        
      } else{
        
        # Voom it up
        count_data_tmp = count_data[,c(a, b)]
        sample_data_tmp = sample_data[c(a, b),]
        model_matrix = model.matrix(~0 + as.factor(sample_data_tmp[,column]))
        colnames(model_matrix) = c(group1_filter, group2_filter)
        count_data_voomed = voom(count_data_tmp, model_matrix)
        
        # Contrasts
        contrasts_cmd = paste0('makeContrasts(', group1_filter, 
                               '-', group2_filter, ', levels = model_matrix)')
        contrasts = eval(parse(text=contrasts_cmd))
        
        # Differential expression
        fit = lmFit(count_data_voomed, model_matrix)
        fit2 = contrasts.fit(fit, contrasts = contrasts)
        fit2 = eBayes(fit2)
        deg_tab = topTable(fit2, number = 100000, adjust = 'fdr')
        deg_tab = data.frame(Gene = row.names(deg_tab), deg_tab)
        deg_tab$Gene = as.character(deg_tab$Gene)
        deg_tab = deg_tab[order(deg_tab$adj.P.Val, decreasing = F),]
        
        # Save table to prevent re-creating it in the same session
        write.table(deg_tab, out_filename, sep = ',', row.names = F, quote = F)
        
        # Save methods file
        generate_volcano_methods(group1_filter, length(a), 
                                 group2_filter, length(b))
        
      }
      diffExprData$diff_expr_result <- deg_tab
    }
  })
  
  output$volcano_message <- renderUI({
    if (is.null(diffExprData$diff_expr_result)){
      return(p(style='padding-left: 20px; color: #D2D6DD;', 
               'Use the section to the left to set analysis parameters and click "Run analysis" to view results'))
    } else{
      return(p(style='padding-left: 20px; color: #D2D6DD;',
               'Hover over points to view gene info'))
    }
  })
  
  output$volcano_plot <- renderPlotly({
    
    group1_filter = group1_criteria()
    group2_filter = group2_criteria()
    
    deg_results = most_recent_result()
    p_cutoff = 0.05
    upper_fc_cutoff = 1
    lower_fc_cutoff = -1
    
    if (!is.null(deg_results)){
      
      deg_results$log10pval = -log10(deg_results$adj.P.Val)
      deg_results$sig = ifelse(deg_results$adj.P.Val < p_cutoff & deg_results$logFC > upper_fc_cutoff, 'increased',
                               ifelse(deg_results$adj.P.Val < p_cutoff & deg_results$logFC < lower_fc_cutoff, 'decreased', 
                                      'not significant'))
      
      plot_cols = c(PLOT_COLORS[2], '#3aa4a9', '#C3C5C7')
      names(plot_cols) = c('increased', 'decreased', 'not significant')
      
      # Plot
      p <- plot_ly(data = deg_results, x = ~logFC, y = ~log10pval,
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
      volcano_plot(deg_results$logFC, deg_results$adj.P.Val, 
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
  
  output$message_differential_expression <- renderText({
    if (is.null(diffExprData$diff_expr_result)){
      return("Choose the groups of samples you'd like to compare above, then click 'Run Analysis' to view volcano plot and differentially expressed genes table")
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
      gene_links = sapply(gene_names, function(s){
        # <a href="https://www.w3schools.com" target="_blank">Visit W3Schools</a>
        HTML(paste0("<a href='https://www.genecards.org/cgi-bin/carddisp.pl?gene=", s, "' target='_blank'>", s,"</a>"))
      })
      df$Gene = gene_links
      
      # Display
      return(datatable(df, rownames = F, selection = 'none',
                       style = 'bootstrap', escape = F))
    } else{
      return(NULL)
    }
  })
  
}

# uiFunc instead of ui
shinyApp(uiFunc, server)
