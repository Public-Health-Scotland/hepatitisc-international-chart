#Code to create chart of hepatitis c by board.

############################.
## Global ----
############################.

############################.
##Packages 
library(dplyr) #data manipulation
library(highcharter) #charts
library(shiny) #shiny app
library(phsstyles) #chart colours

#Preparing data - not needed unless new data coming through
library(readr) #for reading in csv
library(janitor) #for data cleaning

#Set filepath
filepath <- "/PHI_conf/ScotPHO/Website/Charts/Health Conditions/Hepatitis C/shiny_data"

data <- read_csv(paste0(filepath, "/hepatitisc_international.csv")) |> 
  mutate_if(is.character, factor) |>  #converting characters into factors
  clean_names()

#Save as rds
saveRDS(data, paste0(filepath, "/hepatitisc_international.rds"))

data <- readRDS(paste0(filepath, "/hepatitisc_international.rds"))


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
                  highchartOutput("column_chart"),
                  p(div(style = "width: 80%; float: left;", #Footer
                        HTML("Source: <a href='https://www.who.int/publications/i/item/9789240091672'>
                             World Health Organisation. 2024. Global Hepatitis Report.</a>")),
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
  
  output$column_chart <- renderHighchart({
    
    #Data for plot
    data_chart <- data |> subset(measure == input$measure)
    
    #y axis title
    yaxistitle <- ifelse(input$measure == "Prevalence", "Prevalence (%)",
                           "Millions of people")

    data_chart |> 
      hchart("column", hcaes(y = value, x = region)) |> 
      hc_colors(c(phs_colors("phs-blue"))) |> 
      hc_xAxis(title = list(text = "Region")) |> 
      hc_yAxis(title = list(text = yaxistitle))
    
    
  })
  
  } # end of server part

############################.
## Calling app ----
############################.

shinyApp(ui = ui, server = server)

##END