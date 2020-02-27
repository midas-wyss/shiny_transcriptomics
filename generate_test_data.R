library(RColorBrewer)

# Generate 3 clusters of samples
color_column = 'group'
shape_column = 'condition_time'
plot_cols = brewer.pal(11, 'Spectral')

sample_df = data.frame(Sample_ID = paste0('Sample_', 1:60),
                       Group = c(rep('Group 1', 20),
                                 rep('Group 2', 20),
                                 rep('Group 3', 20)),
                       Condition = c(rep(c(rep('Control', 10),
                                     rep('Drug', 10)), 3)),
                       Timepoint = rep(rep(c('24 hr', '48 hr'), 10), 3))

write.table(sample_df, '~/Desktop/sample_metadata.csv', sep = ',',
            row.names = F)
                     
umap_df = data.frame(Sample_ID = paste0('Sample_', 1:60),
                     V1 = c(rnorm(10, mean = 0, sd = 1.5),     # Group 1 control
                            rnorm(10, mean = -5, sd = 1.3),    # Group 1 drug
                            rnorm(10, mean = 5, sd = 1),       # Group 2 control
                            rnorm(10, mean = -3, sd = 1.1),    # Group 2 drug
                            rnorm(10, mean = -7, sd = 1),       # Group 3 control
                            rnorm(10, mean = -5, sd = 1.1)),   # Group 3 drug
                     V2 = c(rnorm(10, mean = -4, sd = 1),      # Group 1 control
                            rnorm(10, mean = 3, sd = 1.3),     # Group 1 drug
                            rnorm(10, mean = 2, sd = 1.7),     # Group 2 control
                            rnorm(10, mean = 5, sd = 1.3),     # Group 2 drug
                            rnorm(10, mean = -7, sd = 1),       # Group 3 control
                            rnorm(10, mean = 1, sd = 1.1)))   # Group 3 drug

write.table(umap_df, '~/Desktop/umap_data.csv', sep = ',',
            row.names = F)
