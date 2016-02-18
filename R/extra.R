library(WRTDStidal)
library(dplyr)
library(ggplot2) 
library(gridExtra)
data(mods)

for(i in 1:length(mods)){
  
  lab <- names(mods)[i]
  p <- prdnrmplot(mods[[i]]) +
    ggtitle(lab) + 
    theme_minimal() +
    theme(legend.position = 'none', axis.title = element_blank()) +
    scale_y_continuous(limits = c(0, 1.3))
  assign(paste0('p', i), p)
  
}

ylab <- attr(mods[[1]], 'reslab')

pdf('C:/Users/mbeck/Desktop/sf_res.pdf', height = 8, width = 13, family = 'serif')
grid.arrange(ncol = 2, widths = c(0.02, 1),
  grid::textGrob(ylab, rot = 90), 
  arrangeGrob(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, ncol = 4)
  )
dev.off()
