#Code to create chart of hepatitis c by board.

############################.
## Global ----
############################.

############################.
##Packages 
library(dplyr) #data manipulation
library(plotly) #charts
library(shiny)

#Preparing data - not needed unless new data coming through
#library (readr)
 
#data <- read_csv("./data/hepatitisc_international.csv") %>%
#   mutate_if(is.character, factor) %>%  #converting characters into factors
#   setNames(tolower(names(.)))
 
#saveRDS(data, "./data/hepatitisc_international.rds")

data <- readRDS("./data/hepatitisc_international.rds")

#ScotPHO logo. 
#Needs to be https address or if local in code 64 (the latter does not work with 4.7 plotly)
scotpho_logo <-  list(source ="https://raw.githubusercontent.com/jvillacampa/test/master/scotpho.png",
                      xref = "paper", yref = "paper",
                      x= -0.09, y= 1.16, sizex = 0.26, sizey = 0.20, opacity = 1)

############################.
## Visual interface ----
############################.
#Height and widths as percentages to allow responsiveness
#Using divs as issues with classing css 
ui <- fluidPage(style="width: 650px; height: 500px; ", 
                div(style= "width:100%", #Filter panel
                          h4("Chart 1. Hepatitis C across different regions of the world"),
                  div(style = "width: 50%; float: left;",
                      selectInput("measure", label = "Select a measure type",
                                  choices = c("Prevalence", "Infected population"))
                         )
                ),
                div(style= "width:100%; float: left;", #Main panel
                  plotlyOutput("chart", width = "100%", height = "350px"),
                  p(div(style = "width: 80%; float: left;", #Footer
                        HTML("Source: <a href='https://www.who.int/publications/i/item/global-hepatitis-report-2017'>
                             World Health Organisation. 2017. Global Hepatitis Report.</a>")),
                    div(style = "width: 20%; float: left",
                        downloadLink('download_data', 'Download data'))
                        )
                  )
                ) #Fluid page bracket

############################.
## Server ----
############################.
server <- function(input, output) {
  
  # Allowing user to download data
  output$download_data <- downloadHandler( 
    filename =  'hepatitisc_data.csv', content = function(file) { 
      write.csv(data, file, row.names=FALSE) })
  
  ############################.
  #Visualization
  output$chart <- renderPlotly({

    #Data for plot
    data_chart <- data %>% subset(measure==input$measure)

    #y axis title
    yaxistitle <- ifelse(input$measure == "Prevalence", "Prevalence (%)", 
                         "Millions of people")
    
    plot_ly(data=data_chart, x=~region, y = ~value, 
                    type = "bar",  marker = list(color = '#08519c'),
                    width = 650, height = 350) %>% 
    #Layout
      layout(annotations = list(), #It needs this because of a buggy behaviour
           yaxis = list(title = yaxistitle, rangemode="tozero", fixedrange=TRUE), 
           xaxis = list(title = "",  fixedrange=TRUE,
                        categoryorder="array", #order of plotting
                        categoryarray = ~value),  
           font = list(family = 'Arial, sans-serif'), #font
           margin = list(pad = 4, t = 30, b = 100), #margin-paddings
           images = scotpho_logo) %>% 
      config(displayModeBar= T, displaylogo = F, collaborate=F, editable =F) # taking out plotly logo and collaborate button

    }) 
  
  } # end of server part

############################.
## Calling app ----
############################.

shinyApp(ui = ui, server = server)

##END