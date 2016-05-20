# packages to use
library(WRTDStidal)
library(ggplot2)
library(dplyr)
library(tidyr)

# raw data
load(file = 'mods_nolag.RData')

# Define server logic required to generate and plot data
shinyServer(function(input, output) {
  
  ##
  # data
  
  # model
  dat <- reactive({

    stat <- input$stat
    res <- input$res
    scl <- input$scl
    
    out <- filter(mods_nolag, Site_Code == stat & resvar == res) %>% 
      .$mod
    out <- out[[1]]
    
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
      
      ylab <- 'ln-salinity'
      
      if(scl == 'linear'){
        
        flo$flo <- exp(flo$flo) - 1
        ylab <- 'salinity'
          
        }
          
    } else {
     
      ylab <- 'ln-flow'
      
      if(scl == 'linear'){
      
        flo$flo <- exp(flo$flo)
        ylab <- 'flow'
        
      }
      
    }
  
    ggplot(flo, aes(x = date, y = flo)) + 
      geom_line() +
      scale_y_continuous(ylab) +
      theme_minimal() +
      theme(axis.title.x = element_blank()) +
      scale_x_date(limits = dt_rng)
        
    }, height = 250, width = 1200)
  
  # predictions and flow norms plot
  output$fitplot <- renderPlot({
    
    # inputs
    dt_rng <- input$dt_rng
    taus <- input$tau
    
    # scale argument
    logspace <- TRUE
    if(input$scl == 'linear') logspace <- FALSE
    
    # aggregation period
    annuals <- TRUE
    if(input$annuals == 'observed') annuals <- FALSE

    # create plot
    fitplot(dat(), annuals = annuals, tau = taus, dt_rng = dt_rng, size = 3, alpha = 0.8, min_mo = 11, 
        logspace = logspace) +
      theme_minimal() +
      theme(legend.position = 'none',
        axis.title.x = element_blank()
        )

    }, height = 250, width = 1200)
  
  # predictions and flow norms plot
  output$nrmplot <- renderPlot({
    
    # inputs
    
    dt_rng <- input$dt_rng
    taus <- input$tau

    # scale argument
    logspace <- TRUE
    if(input$scl == 'linear') logspace <- FALSE
   
    # aggregation period
    annuals <- TRUE
    if(input$annuals == 'observed') annuals <- FALSE
    
    # create plot
    fitplot(dat(), annuals = annuals, predicted = F, tau = taus, dt_rng = dt_rng, size = 3, alpha = 0.8, 
        min_mo = 11, logspace = logspace) + 
      theme_minimal() +
      theme(legend.position = 'none', 
        axis.title.x = element_blank()
        )

    }, height = 250, width = 1200)

  
})