library(WRTDStidal)
library(dplyr)
library(ggplot2) 
library(gridExtra)
source('R/funcs.R')

data(mods)

for(i in 1:length(mods)){
  
  lab <- names(mods)[i]
  p <- seasplot(mods[[i]], alpha = 0.5, size = 1) +
    ggtitle(lab) + 
    theme_minimal() +
    theme(axis.title = element_blank()) #+
    scale_y_continuous(limits = c(0, 1.3))
    
  # get legend
  if(i == 1) pleg <- g_legend(p)
  p <- p + theme(legend.position = 'none')
  
  assign(paste0('p', i), p)
  
}

ylab <- attr(mods[[1]], 'reslab')

pdf('C:/Users/mbeck/Desktop/sf_seas.pdf', height = 8, width = 13, family = 'serif')
grid.arrange(
  ncol = 1, 
  arrangeGrob(
    ncol = 2, widths = c(0.02, 1),
    grid::textGrob(ylab, rot = 90), 
    arrangeGrob(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, pleg, ncol = 4)
  ), 
  grid::textGrob('Day of year'), 
  heights = c(1, 0.025)
)
dev.off()
