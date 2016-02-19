library(WRTDStidal)
library(dplyr)
library(ggplot2) 
library(gridExtra)
source('R/funcs.R')

data(mods)
pdf('C:/Users/mbeck/Desktop/sf_dyna.pdf', height = 8, width = 13, family = 'serif')
for(mo in 1:12){

for(i in 1:length(mods)){
  
  lab <- names(mods)[i]
  p <- dynaplot(mods[[i]], month = mo) +
    ggtitle(lab) + 
    theme_minimal() +
    theme(axis.title = element_blank()) +
    scale_y_continuous(limits = c(0, 1.4))
    
  # get legend
  if(i == 1) pleg <- g_legend(p)
  p <- p + theme(legend.position = 'none')
  
  assign(paste0('p', i), p)
  
}

ylab <- attr(mods[[1]], 'reslab')


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

}
dev.off()
