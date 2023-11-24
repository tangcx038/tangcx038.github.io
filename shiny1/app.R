library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
# Reading the CSV files
waste_03_22 <- read.csv("2003_2022_waste.csv")

energy_stat <- read.csv("waste_energy_stat.csv")


# Renaming columns and mutating the data to convert values from kilo-tonnes to tonnes
clean_waste_03_22 <- waste_03_22 %>%
  rename(
    waste_type = "waste_type",
    total_waste_generated_tonne = "total_generate_1k_tonnes",
    total_waste_recycled_tonne = "total_recycled_1k_tonnes",
    total_waste_not_recycled_tonne = "total_not_recycled_1k_tonnes",
    year = "year"
  ) %>%
  mutate(
    total_waste_generated_tonne = total_waste_generated_tonne * 1000,
    total_waste_recycled_tonne = total_waste_recycled_tonne * 1000
  )

# Filtering waste_03_22 dataset for specific years and waste types
wasteselected <- clean_waste_03_22 %>%
  filter(year %in% 2003:2022,
         waste_type %in% c("Plastics", "Ferrous Metals", "Non-Ferrous Metals", "Glass", "Food"))%>%
  arrange(waste_type)


### Calculating and adding recycling rate into DataFrame for analysis.

# Calculate recycling_rate and round to 2 decimal places
clean_waste_03_22$recycling_rate <- round(
  clean_waste_03_22$total_waste_recycled_tonne / clean_waste_03_22$total_waste_generated_tonne,2
)

### Calculating and adding wasting rate into DataFrame for analysis.

# Calculate wasting_rate and round to 2 decimal places
clean_waste_03_22$wasting_rate <- round(
  1-clean_waste_03_22$recycling_rate,2
)

# Reading the CSV files
waste_03_22 <- read.csv('2003_2022_waste.csv')

# Renaming columns and mutating the data to convert values from kilo-tonnes to tonnes
clean_waste_03_22 <- waste_03_22 %>%
  rename(
    waste_type = "waste_type",
    total_waste_generated_tonne = "total_generate_1k_tonnes",
    total_waste_recycled_tonne = "total_recycled_1k_tonnes",
    total_waste_not_recycled_tonne = "total_not_recycled_1k_tonnes",
    year = "year"
  ) %>%
  mutate(
    total_waste_generated_tonne = total_waste_generated_tonne * 1000,
    total_waste_recycled_tonne = total_waste_recycled_tonne * 1000
  )

# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("Waste Visualization App"),
  sidebarLayout(
    sidebarPanel(
      selectInput("material", "Choose Material:", 
                  choices = unique(clean_waste_03_22$waste_type),
                  selected = "Plastics")
    ),
    mainPanel(
      plotlyOutput("waste_plot")
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
  output$waste_plot <- renderPlotly({
    gg <- ggplot(selected_data(), aes(x = year)) +
      geom_line(aes(y = total_waste_generated_tonne, color = "Waste Generated"), size = 1) +
      geom_line(aes(y = total_waste_recycled_tonne, color = "Waste Recycled"), size = 1) +
      geom_line(aes(y = total_waste_not_recycled_tonne, color = "Waste Not Recycled"), size = 1) +
      labs(title = paste("Waste Over Years -", input$material),
           x = "Year", y = "Tonnes") +
      theme_minimal()
    
    ggplotly(gg, tooltip = "text")
  })
}

# Run the Shiny app
shinyApp(ui, server)