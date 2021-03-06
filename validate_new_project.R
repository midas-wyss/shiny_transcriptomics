library(umap)

NEW_PROJECT_METADATA = 'syn21785559'
NEW_PROJECT_COUNTS = 'syn21785560'

# Login to synapse
reticulate::use_virtualenv('python35_txn_env', required = T)
reticulate::source_python('connect_to_synapse.py')
login_to_synapse(username = Sys.getenv('SYN_USERNAME'),
                 api_key = Sys.getenv('SYN_API_KEY'))

# Download sample metadata to confirm format
sample_metadata_csv = fetch_synapse_filepath(NEW_PROJECT_METADATA)
sample_metadata_df <- read.csv(sample_metadata_csv, stringsAsFactors = F)
names(sample_metadata_df)[1] == 'Sample_ID'
names(sample_metadata_df)

# Correct issues
#names(sample_metadata_df) = c('Sample_ID', 'Group')
#write.table(sample_metadata_df, '~/Desktop/AZM_Transcriptomics_metadata.csv',
#            sep = ',', row.names = F)

# Download count data to confirm format
gene_counts_csv = fetch_synapse_filepath(NEW_PROJECT_COUNTS)
gene_counts_df <- read.csv(gene_counts_csv, stringsAsFactors = F)
names(gene_counts_df)[1] == 'Gene_Symbol'
all(sample_metadata_df$Sample_ID %in% names(gene_counts_df))

# Correct issues
# names(gene_counts_df)[1] = 'Gene_Symbol'
#write.table(gene_counts_df, '~/Desktop/AZM_Transcriptomics_counts.csv',
#            sep = ',', row.names = F)

# Compute UMAP and save
row.names(gene_counts_df) = make.names(gene_counts_df$Gene_Symbol, unique=T)
gene_counts_df$Gene_Symbol = NULL

# Optionally, change number of neighbors if < 15 samples
custom.settings = umap.defaults
custom.settings$n_neighbors = 5
u = umap(t(gene_counts_df), config = custom.settings)
#u = umap(t(gene_counts_df))
uu = as.data.frame(u$layout)
uu = cbind(Sample_ID = row.names(uu), uu)
uu$Sample_ID = as.character(uu$Sample_ID)
plot(uu$V1, uu$V2)

write.table(uu, '~/Desktop/AZM_Transcriptomics_umap.csv', sep = ',', row.names = F, quote = F)

# Confirm UMAP on Synapse looks good
UMAP = 'syn21785561'
umap_csv = fetch_synapse_filepath(UMAP)
umap_df <- read.csv(umap_csv, row.names = 1, stringsAsFactors = F)
all(row.names(umap_df) %in% sample_metadata_df$Sample_ID)
