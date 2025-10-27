library(shiny)
library(dplyr)
library(ggplot2)
library(macleish)

# Combine datasets and create a proper Date column
macleish_weather <- bind_rows(
  whately_2015 %>% mutate(station = "whately_2015"),
  orchard_2015 %>% mutate(station = "orchard_2015")
) %>%
  mutate(date = as.Date(when))  # Replace 'when' with the actual date column name

# Define UI
ui <- fluidPage(
  titlePanel("Macleish Weather Time Series"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "station",
        label = "Select station:",
        choices = c("whately_2015", "orchard_2015"),
        selected = "whately_2015"
      ),
      dateRangeInput(
        inputId = "dateRange",
        label = "Select date range:",
        start = min(macleish_weather$date, na.rm = TRUE),
        end = max(macleish_weather$date, na.rm = TRUE),
        min = min(macleish_weather$date, na.rm = TRUE),
        max = max(macleish_weather$date, na.rm = TRUE)
      )
    ),
    
    mainPanel(
      plotOutput("timeSeriesPlot")
    )
  )
)

# Define server
server <- function(input, output) {
  
  filteredData <- reactive({
    req(input$station, input$dateRange)  # ensure inputs exist
    
    macleish_weather %>%
      filter(
        station == input$station,
        date >= input$dateRange[1],
        date <= input$dateRange[2]
      )
  })
  
  output$timeSeriesPlot <- renderPlot({
    data <- filteredData()
    
    validate(
      need(nrow(data) > 0, "No data available for selected station/date range")
    )
    
    ggplot(data, aes(x = date)) +
      geom_line(aes(y = temperature, color = "Temperature")) +
      theme_minimal(base_size = 14) +
      labs(
        x = "Date",
        y = "Temperature (°C)",
        color = "Legend",
        title = paste("Weather at", input$station)
      )
  })
}

# Run app
shinyApp(ui = ui, server = server)
