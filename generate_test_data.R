# Generate 3 clusters of samples
color_column = 'group'
shape_column = 'condition_time'
plot_cols = brewer.pal(11, 'Spectral')

plot_df = data.frame(well_name = 1:61,
                     group = c(rep('Group 1', 20),
                               rep('Group 2', 20),
                               rep('Group 3', 20),
                               'Group 2'),
                     condition_time = c(rep(c(rep('Control', 10),
                                       rep('Drug', 10)), 3),
                                       'Drug'),
                     V1 = c(rnorm(10, mean = 0, sd = 1.5),      # Group 1 control
                            rnorm(10, mean = -5, sd = 1.3),     # Group 2 drug
                            rnorm(10, mean = 5, sd = 1),        # Group 2 control
                            rnorm(10, mean = -3, sd = 1.1),     # Group 2 drug
                            rnorm(10, mean = -7, sd = 1.5),     # Group 3 control
                            rnorm(10, mean = -7, sd = 1.1),     # Group 3 drug
                            -1),
                     V2 = c(rnorm(10, mean = -4, sd = 1),      # Group 1 control
                            rnorm(10, mean = 3, sd = 1.3),     # Group 1 drug
                            rnorm(10, mean = 2, sd = 1.7),     # Group 2 control
                            rnorm(10, mean = 5, sd = 1.3),     # Group 2 drug
                            rnorm(10, mean = -4, sd = 1),      # Group 3 control
                            rnorm(10, mean = 2, sd = 1.5),     # Group 3 drug
                            -6.5))

plot_ly(data = plot_df, x = ~V1, y = ~V2,
        color = ~get(color_column), 
        colors = plot_cols[c(2,9,10)],
        symbol = ~get(shape_column),
        symbols = 21,
        text = ~well_name,
        hovertemplate = '<b>Sample ID:</b> %{text}',
        type = 'scatter', mode = 'markers',
        marker = list(size = 10,
                      line = list(
                        color = '#212D32',
                        width = 1
                      ))) %>%
    layout(title = 'UMAP plot')
