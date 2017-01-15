# packages to use
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(WRTDStidal)
library(RCurl)

# Define server logic required to generate and plot data
shinyServer(function(input, output) {
  
  ##
  # data
  
  # model
  dat <- reactive({

    stat <- input$stat
    res <- input$res

    fl <- paste0(stat, '_', res)
    downloadURL <- paste0('https://s3.amazonaws.com/sftrends/', fl, '.RData')
    bin <- getBinaryURL(downloadURL)
    writeBin(bin, "temp.rData")  
    load("temp.rData")
    out <- get(fl)

    return(out)
    
  })
  
  # for initial date range
  output$daterng <- renderUI({

    rngs <- range(dat()$date)

    dateRangeInput("dt_rng",
      label = h4("Date range"), 
      start = rngs[1], 
      end = rngs[2],
      startview = 'year'
    )
    
  })
  
  ## plots

  # floplot
  output$floplot <- renderPlot({
    
    # inputs
    dt_rng <- input$dt_rng
    scl <- input$scl
    stat <- input$stat
    
    # data
    flo <- dat()
    florng <- attr(flo, 'floobs_rng')
    flolab <- attr(flo, 'flolab')
    flo <- dplyr::select(flo, date, flo) %>% 
      mutate(flo = flo * abs(diff(florng)) + florng[1]) %>% 
      na.omit

    # change scale/labels depending on station and scale
    if(stat %in% c('D4', 'D6', 'D7')){
      
      ylab <- 'ln-Salinity'
      
      if(scl == 'linear'){
        
        flo$flo <- exp(flo$flo) - 1
        ylab <- 'Salinity'
          
        }
          
    } else {
     
      ylab <- paste('ln-Flow', flolab, sep = ', ')
      
      if(scl == 'linear'){
      
        flo$flo <- exp(flo$flo)
        ylab <- paste('Flow', flolab, sep = ', ')
        
      }
      
    }
  
    ggplot(flo, aes(x = date, y = flo)) + 
      geom_line() +
      scale_y_continuous(ylab) +
      theme_bw() +
      theme(axis.title.x = element_blank()) +
      scale_x_date(limits = dt_rng)
        
    }, height = 240, width = 1200)
  
  # predictions and flow norms plot
  output$fitplot <- renderPlot({
    
    # inputs
    dt_rng <- input$dt_rng
    taus <- input$tau
    remobs <- input$remobs
    if(remobs) size <- NA
    else size <- 3
    
    # scale argument
    logspace <- TRUE
    if(input$scl == 'linear') logspace <- FALSE
    
    # aggregation period
    annuals <- TRUE
    if(input$annuals == 'observed') annuals <- FALSE

    # create plot
    fitplot(dat(), annuals = annuals, tau = taus, dt_rng = dt_rng, size = size, alpha = 0.8, min_mo = 11, 
        logspace = logspace, col_vec = 'darkblue') +
      theme_bw() +
      theme(legend.position = 'none',
        axis.title.x = element_blank()
        ) + 
      ggtitle('Predictions (lines) and observed (points)')

    }, height = 255, width = 1200)
  
  # predictions and flow norms plot
  output$nrmplot <- renderPlot({
    
    # inputs
    
    dt_rng <- input$dt_rng
    taus <- input$tau
    remobs <- input$remobs
    if(remobs) size <- NA
    else size <- 3

    # scale argument
    logspace <- TRUE
    if(input$scl == 'linear') logspace <- FALSE
   
    # aggregation period
    annuals <- TRUE
    if(input$annuals == 'observed') annuals <- FALSE
 
    # create plot
    fitplot(dat(), annuals = annuals, predicted = F, tau = taus, dt_rng = dt_rng, size = size, alpha = 0.8, 
        min_mo = 11, logspace = logspace, col_vec = 'darkblue') + 
      theme_bw() +
      theme(legend.position = 'none', 
        axis.title.x = element_blank()
        ) + 
      ggtitle('Flow-normalized predictions (lines) and observed (points)')

    }, height = 245, width = 1200)

  
})