library(shiny)

# Define UI for application
shinyUI(fluidPage(
  
  # Application title
  h2("Delta and  Suisun weighted regression results"),
  
  # Sidebar with a slider input for number of observations
  fluidRow(
    
    column(2, 
      
      selectInput(inputId = 'stat',
        label = h4('Station'),
        choices = c('C10', 'C3', 'P8', 'D4', 'D6', 'D7'), 
        selected = 'C10')
      
    ),

    column(2, 
            
      selectInput(inputId = 'res', 
        label = h4('Variable'),
        choices = c('din', 'nh', 'no23'), 
        selected = 'din')
        
    ),
    
    column(2, 
      
      selectInput(inputId = 'annuals', 
        label = h4('Plot type'), 
        choices = c('annual', 'observed'), 
        selected = 'observed'
        )
      
    ),
    
    column(2, 
      
      uiOutput("daterng")
      
    ),
    
    column(2, 
      
      checkboxGroupInput("tau", 
        label = h4("Quantiles"),
        choices = c("0.1" = "0.1", "0.5" = "0.5", "0.9" = "0.9"),
        selected = c('0.1', '0.5', '0.9'), 
        inline = T
        )
    
      ),
    
    width = 12
    
  ),
  
  
  # output tabs
  mainPanel(
    
    plotOutput("floplot", height = "100%"),
    plotOutput("fitplot", height = "100%"),
    plotOutput("nrmplot", height = "100%"),
   
    width = 9
      
  )

))