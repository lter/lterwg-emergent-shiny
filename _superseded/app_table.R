#This is the Shiny app for the Neon data showcasing the tables of the data
#Author: Dvir Blander and Katrina Newcomer
#First loading in the shiny, dplyr, readr libraries.
#The shiny library is used because this is a shiny app.
#The dplyr and readr libraries are used to help read in the data. 
#The DT library is used for the datatables
library(plotly)
library(ggplot2)
library(shiny)
library(dplyr)
library(readr)
library(DT)
library(shinyWidgets)
#Note: The files are loaded onto the local machine. The folder should be on GitHub and it's name is NeonFiles.
#Make sure to set the working directory as the GitHub "NeonFiles" folder.
#This can be done by clicking Session --> Set Working Directory --> Choose Directory. Then navigate to this directory.

#Loading in the csv files and showing less than 113 columns
soilFieldChem <- read.csv(file = file.path('data', 'table_data.csv'))

# UI ----
ui <- fluidPage(
  titlePanel("Neon Data Table"),
  sidebarLayout(position = "left",
                tabPanel("Table",
                         sidebarPanel(
                           selectInput("selection1", 
                                       label = h3("Select nlcdClass"),
                                       choices =  c("choose" = "", unique(soilFieldChem$nlcdClass)), 
                                       selected = 'mixedForest' ),
                           selectInput("selection2", 
                                       label = h3("Select siteID"),
                                       choices = c("choose" = "", unique(soilFieldChem$siteID)),
                                       selected = 'BART'),
                           selectInput("selection4",
                                       label = h3("Select biophysicalCriteria"), 
                                       choices = c("choose" = "", unique(soilFieldChem$biophysicalCriteria)),
                                       selected = 'OK - no known exceptions'),
                           selectInput("selection5", label = h3("Select sampleTiming"), 
                                       choices = c("choose" = "", unique(soilFieldChem$sampleTiming)), 
                                       selected ='peakGreenness')
                         )
                         
                         
                ),
                mainPanel(DT::dataTableOutput("table"))
  )
)


server <- function(input, output) {
  
  tab <- reactive({ 
    
    soilFieldChem %>% 
      filter(nlcdClass == input$selection1) %>% 
      filter(siteID == input$selection2) %>%
      filter(biophysicalCriteria == input$selection4) %>%
      filter(sampleTiming == input$selection5 )
    
  })
  output$table <-DT::renderDataTable({
    tab() })
} 

# Create Shiny app objects from either an explicit UI/server pair 
shinyApp(ui = ui, server = server)
