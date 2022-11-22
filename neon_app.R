## ------------------------------------------ ##
            # NEON Chemistry App
## ------------------------------------------ ##
# Authors: Dvir Blander, Katrina Newcomer, & Nick Lyon

# Housekeeping ----

# Load libraries
library(tidyverse); library(plotly) # Wrangling / plotting
library(shiny); library(htmltools); library(DT); library(shinyWidgets) # Shiny et al.

# Load data
table_data <- read.csv(file = file.path("data", "table_data.csv"))

# User Interface (UI) ----
neon_ui <- fluidPage(
  
  titlePanel(title = "NEON Chemistry Data Shiny App"),
  
  # Make app into multiple tabs
  tabsetPanel(
    
  # UI - Data Table Tab ----
  tabPanel(title = "NEON Data Table",
  
    # Build sidebar with dropdown menus to choose which data to display
    sidebarLayout(position = "left",
      
      # Sidebar
      sidebarPanel(
        
        # Dropdowns
        selectInput(inputId = "dd_nlcdClass",
                    label = htmltools::h3("Select nlcdClass"),
                    choices = unique(table_data$nlcdClass),
                    selected = unique(table_data$nlcdClass)[1]),
        selectInput(inputId = "dd_siteID",
                    label = htmltools::h3("Select siteID"),
                    choices = unique(table_data$siteID),
                    selected = unique(table_data$siteID)[1]),
        selectInput(inputId = "dd_biophysicalCriteria",
                    label = htmltools::h3("Select biophysicalCriteria"),
                    choices = unique(table_data$biophysicalCriteria),
                    selected = unique(table_data$biophysicalCriteria)[1]),
        selectInput(inputId = "dd_sampleTiming",
                    label = htmltools::h3("Select sampleTiming"),
                    choices = unique(table_data$sampleTiming),
                    selected = unique(table_data$sampleTiming)[1])
      ), # Close sidebar
      
      # Main panel
      mainPanel(DT::dataTableOutput(outputId = "table_out",
                                    width = "100%", height = "auto"))
        
    ) # Close sidebarLayout
    ), # Close tabPanel
  
  # UI - Graphs Tab ----
  tabPanel(title = "NEON Graphs",
           
           # Build sidebar with dropdown menus to choose which data to display
           sidebarLayout(position = "left",
                         sidebarPanel(
                           selectInput(inputId = "dd_habitat",
                                       label = htmltools::h3("Select Habitat Type"),
                                       choices = c("All", unique(table_data$nlcdClassSimple)),
                                       selected = "All"),
                           selectInput(inputId = "dd_response",
                                       label = htmltools::h3("Select Response Variable"),
                                       choices = c("soilTemp", "soilMoisture"),
                                       selected = "soilTemp")
                         ),
                         
                         # Main panel
                         mainPanel(DT::plotOutput(outputId = "soil_plot"))
           ) # Close sidebarLayout
  ) # Close tabPanel
  ) # Close tabsetPanel
) # Close fluidPage

# Server ----
neon_server <- function(input, output){
  
  # Server - Table Tab ----
  # Subset table as directed by user
  table_sub <- reactive({
    table_data %>%
      dplyr::filter(nlcdClass == input$dd_nlcdClass) %>%
      dplyr::filter(siteID == input$dd_siteID) %>%
      dplyr::filter(biophysicalCriteria == input$dd_biophysicalCriteria) %>%
      dplyr::filter(sampleTiming == input$dd_sampleTiming)
  })
  
  # Render the table
  output$table_out <- DT::renderDataTable({ table_sub() })
  
  # Server - Graph Tab ----
  # Summarize the data
  mean_soil <- table_data %>%
    dplyr::group_by(siteID, nlcdClassSimple) %>%
    dplyr::summarize(soilMoisture = mean(soilMoisture, na.rm = T),
                     soilTemp = mean(soilTemp, na.rm = T),
                     .groups = "keep") %>%
    dplyr::ungroup()
  
  # Subset to the selected habitat type
  plot_data <- reactive({
    if(dd_habitat == "All"){ mean_soil } else { dplyr::filter(mean_soil, nlcdClassSimple == dd_habitat) }
  })
  
  # Make the plot
  ggplot(data = plot_data, aes(x = siteID, y = .data[[dd_response]], fill = nlcdClassSimple)) +
    geom_bar(stat = 'identity') +
    geom_errorbar(aes(ymax = dd_response + .data[[paste0(dd_response, "_SD")]],
                              ymin = dd_response - .data[[paste0(dd_response, "_SD")]]),
                   width = 0.2) +
    theme_bw()
    
  
} # Close server function

# Run App ----
shiny::shinyApp(ui = neon_ui, server = neon_server)

# End ----
