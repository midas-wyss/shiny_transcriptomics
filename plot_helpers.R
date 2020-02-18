library(RColorBrewer)

# Plot color palettes
PLOT_COLORS <- brewer.pal(11, 'Spectral')
PLOT_SHAPES <- 21:25

titlify <- function(s){
  to_title <- function(ss){
    ss = paste0(toupper(substr(ss,1,1)), substr(ss,2,nchar(ss)))
    ss = gsub('_', ' ', ss)
    ss = gsub(' ic', ' IC', ss)
    return(ss)
  }
  if (length(s) > 1){
    return(sapply(s, to_title))
  } else{
    return(to_title(s))
  }
}

volcano_plot <- function(log2_fc, p_adj, p_adj_threshold = 0.01, 
                         plot_main = 'Volcano', ymax = 25){
  
  if (length(log2_fc) != length(p_adj)){
    return('Log2FC and pAdj vectors are different lengths!')
  } else {
    
    # Red, teal, grey
    plot_cols = c(PLOT_COLORS[2], '#3aa4a9', '#C3C5C7')
    cols = rep(plot_cols[3], length(p_adj))
    cols[log2_fc > 1 & p_adj < p_adj_threshold] = plot_cols[1]
    cols[log2_fc < -1 & p_adj < p_adj_threshold] = plot_cols[2]
    
    plot(log2_fc, -log10(p_adj),
         main = plot_main,
         xlab = 'log2 fold change', ylab = '-log10 FDR-adjusted p-value',
         ylim = c(0, ymax),
         las = 1, pch = 21,
         bg = cols)
    abline(h = -log10(p_adj_threshold),
           col = plot_cols[3])
    abline(v = 1, col = plot_cols[1])
    abline(v = -1, col = plot_cols[2])
  }
}