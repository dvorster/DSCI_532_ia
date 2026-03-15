library(shiny)
library(ggplot2)
library(maps)
library(tidyverse)
library(DT) # For interactive tables

# Load USA state map data
df <- read_csv("data/processed/processed.csv")
df <- df %>%
  filter(state_id != "HI")

# Pre-process unique cities for the dropdown
all_cities <- sort(unique(df$city))

# Load USA map data
us_map_data <- map_data("state")

# Define UI
ui <- fluidPage(
  titlePanel("US Crime Analytics Dashboard"),
  
  # Filter Section
  fluidRow(
    column(12, 
           wellPanel(
             fluidRow(
               column(4, 
                      sliderInput("select_year", "Select Year:", 
                                  min(df$year), max(df$year), min(df$year), 
                                  sep = "", step = 1, width = "100%")
               ),
               column(8,
                      selectizeInput("city_select", "Select Cities (Leave blank for National Total):", 
                                     choices = all_cities, selected = NULL, multiple = TRUE, width = "100%",
                                     options = list(placeholder = 'Showing All Cities', plugins = list("remove_button")))
               )
             ),
           )
    )
  ),
  
  # Main Display Area
  fluidRow(
    column(12, 
           plotOutput("us_map", height = "600px") 
    )
  ),
  
  hr(),
  fluidRow(
    column(10, offset = 1, 
           h4("City Data Detail"),
           DTOutput("crime_table")
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  
  filtered_data <- reactive({
    req(input$select_year)
    
    # Start with data for the selected year
    data_out <- df %>% filter(year == input$select_year)
    
    # Conditional Filter: Only filter by city IF the input is NOT empty
    if (!is.null(input$city_select) && length(input$city_select) > 0) {
      data_out <- data_out %>% filter(city %in% input$city_select)
    }
    
    return(data_out)
  })
  
  output$us_map <- renderPlot({
    # Create the plot
    ggplot() +
      # background
      geom_polygon(data=us_map_data, 
                   aes(x = long, y = lat, group = group),
                   fill = "lightgrey", color = "white") +
      # City bubbles
      geom_point(data = filtered_data(), 
                 aes(x = lng, y = lat, size = violent_crime),
                 color = "darkred",
                 alpha = 0.7) +
      scale_size_continuous(range = c(1, 10), name = "Violent Crimes") +
      scale_color_viridis_c(option = "magma", name = "Violent Crimes") +
      coord_fixed(1.3) +
      theme_void() +
      labs(title = "Violent Crime by City (1975)")
  })
  
  # Render the Table
  output$crime_table <- renderDT({
    data_to_show <- filtered_data()
    
    data_to_show <- data_to_show %>% 
      filter(year == input$select_year) %>%
      select(state_id, city, total_pop, violent_crime, violent_per_100k)
    
    datatable(data_to_show, 
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE,
              filter = 'top') # Adds per-column search boxes
  })
}

# Launch App
shinyApp(ui = ui, server = server)