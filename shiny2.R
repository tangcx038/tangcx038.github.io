library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

# Reading the CSV files
clean_waste_03_22 <- read.csv('2003_2022_waste.csv') %>%
  rename(
    waste_type = "waste_type",
    total_waste_generated_tonne = "total_generate_1k_tonnes",
    total_waste_recycled_tonne = "total_recycled_1k_tonnes",
    total_waste_not_recycled_tonne = "total_not_recycled_1k_tonnes",
    year = "year"
  ) %>%
  mutate(
    total_waste_generated_tonne = total_waste_generated_tonne * 1000,
    total_waste_recycled_tonne = total_waste_recycled_tonne * 1000,
    recycling_rate = round(total_waste_recycled_tonne / total_waste_generated_tonne, 2),
    wasting_rate = round(1 - recycling_rate, 2)
  )

# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("Recycling and Wasting Rates Visualization App"),
  sidebarLayout(
    sidebarPanel(
      selectInput("material", "Choose Material:",
                  choices = unique(clean_waste_03_22$waste_type),
                  selected = "Plastics")
    ),
    mainPanel(
      plotlyOutput("rate_plot")
    )
  )
)

# Define server logic for the Shiny app
server <- function(input, output) {
  
  # Filter data based on selected material
  selected_data <- reactive({
    clean_waste_03_22 %>%
      filter(waste_type == input$material)
  })
  
  # Render the plot
  output$rate_plot <- renderPlotly({
    gg <- ggplot(selected_data(), aes(x = year)) +
      geom_line(aes(y = recycling_rate, color = "Recycling Rate"), size = 1) +
      geom_line(aes(y = wasting_rate, color = "Wasting Rate"), size = 1) +
      labs(title = paste("Recycling and Wasting Rates Over Years -", input$material),
           x = "Year", y = "Rate") +
      theme_minimal()
    
    ggplotly(gg, tooltip = "text")
  })
}

# Run the Shiny app
shinyApp(ui, server)