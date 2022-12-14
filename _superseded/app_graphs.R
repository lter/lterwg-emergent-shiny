#This is the Shiny app for the Neon data Graphs
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

#Loading in the csv files
soilFieldChem <- read.csv(file = file.path('data', 'table_data.csv'))
grass <- soilFieldChem[grep('grassland|Grassland', soilFieldChem$nlcdClass), ]
forest <- soilFieldChem[grep('forest|Forest', soilFieldChem$nlcdClass), ]

forestsub <- forest %>%
  group_by(siteID, nlcdClass) %>%
  summarise(mean_soilMoisture = mean(soilMoisture, na.rm = TRUE),
            mean_soilTemp = mean(soilTemp, na.rm = TRUE), 
            .groups="keep")

grasssub <- grass %>%
  group_by(siteID, nlcdClass) %>%
  summarise(mean_soilMoisture = mean(soilMoisture, na.rm = TRUE),
            mean_soilTemp = mean(soilTemp, na.rm = TRUE), .groups="keep")

allsub <- soilFieldChem%>% 
  group_by(siteID, nlcdClass) %>% 
  summarise(mean_soilMoisture = mean(soilMoisture, na.rm = TRUE),
            mean_soilTemp = mean(soilTemp, na.rm = TRUE))


# UI ----
ui <- fluidPage(
  titlePanel("Neon Graphs"),
  sidebarLayout(position = "left",
                sidebarPanel(
                  selectInput(inputId = "selection", 
                              label = h3("Select Type of Site"),
                              choices = c("All Sites", "Forrested Sites", "Grassland Sites"),
                              selected = 1),
                  selectInput(inputId = "selection3",
                              label = h3("Soil Temp or Moisture"),
                              choices = c("soilTemp", "soilMoisture"),
                              selected = 1)),
                
                # mainPanel(plotOutput("distPlot2"))
                mainPanel(plotOutput("distPlot"))
  ) )

# Server ----
server <- function(input, output) {
  
  output$select_s1 <- renderUI({
    selectizeInput('s1', 'Select variable 1', 
                   choices = c("select" = "", unique(soilFieldChem$selection1)))
  })
  
  output$select_selection2 <- renderUI({
    choice_selection2 <- reactive({
      soilFieldChem %>%
        dplyr::filter(selection1 == input$s1) %>%
        dplyr::pull(selection2) %>%
        as.character()
      })
  })
  
  output$selection4 <- renderUI({
    choice_selection4 <- reactive({
      soilFieldChem %>%
        filter(selection1 == input$s1) %>%
        filter(selection2 == input$s2) %>%
        pull(selection4) %>%
        as.character()
      })
  })

  output$distPlot <- renderPlot({
        if (input$selection == "All Sites") { x <- soilFieldChem }
        else if (input$selection == "Grassland Sites" ) { x <- grass }
        else if (input$selection == "Forrested Sites" ) { x <- forest }

    ggplot(data = x, aes(x=siteID, y= !!sym(input$selection3))) +
      geom_boxplot() +
      ylim(c(-20, 50)) +
      theme(axis.text.x = element_text(angle=45)) +
      ggtitle("for Each Site ID")
    })
  
    output$distPlot2 <- renderPlot ({
      if (input$selection == "All Sites") {  x <- soilFieldChem }
      else if (input$selection == "Grassland Sites" ) { x <- grass }
      else if (input$selection == "Forrested Sites" ) { x <- forest }
      
      xsub <- x %>%
        group_by(siteID, nlcdClass) %>%
        summarise(mean_soilMoisture = mean(soilMoisture, na.rm = TRUE),
                  mean_soilTemp = mean(soilTemp, na.rm = TRUE), .groups="keep")
      
      g1 <- ggplot(data = xsub, aes(x=mean_soilMoisture, y=mean_soilTemp, color = siteID, label = siteID)) +
        geom_point() + geom_text(aes(label=siteID),hjust=-0.2, vjust=0.5) +
        xlim(c(0, 4)) +
        ylim(c(0, 30)) +
        ggtitle('Soil Moisture x Temperatue Forested Plots')
    })
}

# Create Shiny app objects from either an explicit UI/server pair 
shinyApp(ui = ui, server = server)

# End ----
