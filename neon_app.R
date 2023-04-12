## ------------------------------------------ ##
            # NEON Chemistry App
## ------------------------------------------ ##
# Authors: Dvir Blander, Katrina Newcomer, & Nick Lyon

# Housekeeping ----

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, shiny, htmltools, DT, shinyWidgets, lterpalettefinder)

# Load data
## Note that "app_data.csv" is created by running "neon_wrangle.R"
## You *must* run "neon_wrangle" until "app_data.csv" is written into the "data" folder
table_data <- read.csv(file = file.path("data", "app_data.csv"))

# Grab a palette for use in plots
full_palette <- lterpalettefinder::palette_find(type = "qualitative", name = "lotus",
                                                site = "HBR")

# Grab a palette for the soil data
nlcd_palette <- lterpalettefinder::palette_subsample(palette = full_palette,
                                                     wanted = length(unique(table_data$nlcdClass)))

# User Interface (UI) ----
neon_ui <- fluidPage(
  
  # Global app title
  titlePanel(title = "NEON Chemistry Data Shiny App"),
  
  # Make app into multiple tabs
  tabsetPanel(
    
    # UI - Overview Tab ----
    tabPanel(title = "Overview",
             
             # Add informative text
             htmltools::h2("Ecological Metagenome-derived Reference Genomes and Traits (EMERGENT)"),
             
             # Add more text
             "Our climate crisis, resulting from changes in interacting climate variables (temperature, rainfall, atmospheric chemistry) over the last century, has impacted all ecosystems on the surface of the Earth. The Ecological Metagenome-derived Reference Genomes and Traits (EMERGENT) synthesis project connects genomic information about the soil microbiome with the broader ecological context. Our work will advance efforts to harmonize molecular information for microbial taxa and their functional traits, streamline their use in syntheses with related ecosystem level data, and enable future metagenomic studies to leverage EDI environmental data, spurring future microbial ecology research at LTER sites.",
             
             # Line break
             htmltools::br(), htmltools::br(),
             
             # Link to website
             htmltools::strong(
               "For more information on the EMERGENT project, please visit ",
               htmltools::a(href = "https://lter.github.io/lterwg-emergent-website/",
                            "our website.", target = "_blank")),
             
             # Add some line spacing
             htmltools::br(), htmltools::br(),
             
             # Describe app
             "This Shiny app is meant to allow interested parties to explore portions of the datasets that are particularly relevant to the EMERGENT synthesis project's hypotheses and questions.",
             
             # Add more line spacing
             htmltools::br(), htmltools::br(), htmltools::br(),
             
             # Insert logos
             img(src = "neon_logo.png", height = 100, align = "center"),
             img(src = "lter_logo.png", height = 100, align = "center")
             
    ), # Close tabPanel
    
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
    
    # UI - Soil Graphs Tab ----
    tabPanel(title = "Soil Graphs",
             
             # Build sidebar with dropdown menus to choose which data to display
             sidebarLayout(position = "left",
                           sidebarPanel(
                             # Dropdown for habitat selection
                             selectInput(inputId = "dd_habitat_soil",
                                         label = htmltools::h3("Select Habitat Type"),
                                         choices = c("All", unique(table_data$nlcdClass)),
                                         selected = "All"),
                             # Radio buttons for which plot to make
                             radioButtons(inputId = "which_plot_soil",
                                          label = htmltools::h3("Select Desired Plot"),
                                          choices = c("Soil Temp by Site",
                                                      "Soil Moisture by Site",
                                                      "Soil Temp by Soil Moisture"),
                                          selected = "Soil Temp by Site"),
                             # Slider for year range
                             sliderInput(inputId = "soil_years",
                                         label = htmltools::h3("Choose Date Range"),
                                         min = min(table_data$collectYear),
                                         max = max(table_data$collectYear),
                                         step = 1, sep = "",
                                         value = c(min(table_data$collectYear),
                                                   max(table_data$collectYear)) )
                           ),
                           
                           # Main panel
                           mainPanel(
                             plotOutput(outputId = "plot_soil")
                           )
             ) # Close sidebarLayout
    ), # Close tabPanel
    
    # UI - pH Graphs Tab ----
    tabPanel(title = "pH Graphs",
             
             # Build sidebar with dropdown menus to choose which data to display
             sidebarLayout(position = "left",
                           sidebarPanel(
                             # Dropdown for habitat selection
                             selectInput(inputId = "dd_habitat_ph",
                                         label = htmltools::h3("Select Habitat Type"),
                                         choices = c("All", unique(table_data$nlcdClass)),
                                         selected = "All"),
                             # Radio buttons for which plot to make
                             radioButtons(inputId = "which_plot_ph",
                                          label = htmltools::h3("Select Desired Plot"),
                                          choices = c("pH by Site"),
                                          selected = "pH by Site")
                           ),
                           
                           # Main panel
                           mainPanel(
                             htmltools::h1("Plots under construction, check back later!")
                             # plotOutput(outputId = "plot_ph")
                           )
             ) # Close sidebarLayout
    ), # Close tabPanel
  ) # Close tabsetPanel
) # Close fluidPage

# Server ----
neon_server <- function(input, output){
  
  # Server - Table Tab ----
  # Subset table as directed by user
  table_sub <- reactive({
    table_data %>%
      dplyr::filter(nlcdClass == input$dd_nlcdClass) %>%
      dplyr::filter(biophysicalCriteria == input$dd_biophysicalCriteria) %>%
      dplyr::filter(sampleTiming == input$dd_sampleTiming)
  })
  
  # Render the table
  output$table_out <- DT::renderDataTable({ table_sub() })
  
  # Server - Soil Graph Tab ----
  
  # Subset to the selected habitat type
  plot_soil_data_v1 <- reactive({
    if(input$dd_habitat_soil == "All"){ table_data } else { 
      table_data %>% 
        dplyr::filter(nlcdClass == input$dd_habitat_soil) }
  })
  
  # Subset to desired years (from slider)
  plot_soil_data <- reactive({
    plot_soil_data_v1() %>%
      subset(collectYear >= input$soil_years[1] & collectYear <= input$soil_years[2])
  })
  
  # Make the plots
  graph_soil <- reactive({
    # Soil temp ~ site plot
    if(input$which_plot_soil == "Soil Temp by Site"){
      plot_soil_data() %>%
        dplyr::filter(abs(soilTemp) <= 75 & !is.na(soilTemp)) %>%
        ggplot(data = ., aes(x = siteID, y = soilTemp, 
                             color = soilTemp)) +
        geom_jitter(alpha = 0.5, width = 0.25, height = 0) +
        labs(x = "Site ID", y = "Soil Temperature") +
        theme_bw() +
        theme(axis.text.x = element_text(size = 14, angle = 35, hjust = 1),
              axis.text.y = element_text(size = 16),
              legend.text = element_text(size = 15),
              legend.title = element_blank(),
              axis.title = element_text(size = 20))
    } else if (input$which_plot_soil== "Soil Moisture by Site"){
      plot_soil_data() %>%
        dplyr::filter(soilMoisture > -3 & !is.na(soilMoisture)) %>%
        ggplot(data = ., aes(x = siteID, y = soilMoisture, 
                             color = soilMoisture)) +
        geom_jitter(alpha = 0.5, width = 0.25, height = 0) +
        labs(x = "Site ID", y = "Soil Moisture") +
        theme_bw() +
        theme(axis.text.x = element_text(size = 14, angle = 35, hjust = 1),
              axis.text.y = element_text(size = 16),
              legend.text = element_text(size = 15),
              legend.title = element_blank(),
              axis.title = element_text(size = 20))
    } else {
      plot_soil_data() %>%
        dplyr::filter(soilMoisture > -3) %>%
        dplyr::filter(abs(soilTemp) <= 75 & !is.na(soilTemp)) %>%
        dplyr::filter(!is.na(soilTemp) & !is.na(soilMoisture)) %>%
        ggplot(data = ., aes(x = soilTemp, y = soilMoisture, 
                             color = nlcdClass)) +
        geom_point(alpha = 0.4) +
        geom_smooth(method = "lm", formula = "y ~ x", se = F) +
        labs(x = "Soil Temperature", y = "Soil Moisture") +
        scale_fill_manual(values = nlcd_palette) +
        theme_bw() +
        theme(axis.text = element_text(size = 16),
              legend.text = element_text(size = 15),
              legend.title = element_blank(),
              axis.title = element_text(size = 20))
    }
  })
  
  # Render the created soil plot
  output$plot_soil <- renderPlot({ graph_soil() })
  
} # Close server function

# Run App ----
shiny::shinyApp(ui = neon_ui, server = neon_server)

# End ----
