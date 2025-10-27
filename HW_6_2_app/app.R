library(shiny)
library(ggplot2)
library(dplyr)
library(palmerpenguins)

# Convert factors to characters so Shiny shows proper labels
penguins_clean <- penguins %>%
  mutate(
    species = as.character(species),
    sex = as.character(sex)
  )

ui <- fluidPage(
  titlePanel("Palmer Penguins Explorer "),
  
  sidebarLayout(
    sidebarPanel(
      # Select species
      selectInput(
        inputId = "species",
        label = "Select species:",
        choices = c("All species", sort(unique(na.omit(penguins_clean$species)))),
        selected = "All species"
      ),
      
      # Select sex
      selectInput(
        inputId = "sex",
        label = "Select sex:",
        choices = c("All sexes", sort(unique(na.omit(penguins_clean$sex)))),
        selected = "All sexes"
      ),
      
      # X-axis variable
      selectInput(
        inputId = "xvar",
        label = "X-axis variable:",
        choices = c(
          "Bill length (mm)" = "bill_length_mm",
          "Bill depth (mm)" = "bill_depth_mm",
          "Flipper length (mm)" = "flipper_length_mm",
          "Body mass (g)" = "body_mass_g"
        ),
        selected = "bill_length_mm"
      ),
      
      # Y-axis variable
      selectInput(
        inputId = "yvar",
        label = "Y-axis variable:",
        choices = c(
          "Bill depth (mm)" = "bill_depth_mm",
          "Flipper length (mm)" = "flipper_length_mm",
          "Body mass (g)" = "body_mass_g",
          "Bill length (mm)" = "bill_length_mm"
        ),
        selected = "flipper_length_mm"
      )
    ),
    
    mainPanel(
      plotOutput("scatterPlot"),
      br(),
      textOutput("summaryText")
    )
  )
)

server <- function(input, output) {
  
  filteredData <- reactive({
    data <- penguins_clean %>%
      filter(
        !is.na(species),
        !is.na(sex),
        !is.na(!!sym(input$xvar)),
        !is.na(!!sym(input$yvar))
      )
    
    if (input$species != "All species") {
      data <- data %>% filter(species == input$species)
    }
    if (input$sex != "All sexes") {
      data <- data %>% filter(sex == input$sex)
    }
    data
  })
  
  output$scatterPlot <- renderPlot({
    ggplot(filteredData(), aes_string(x = input$xvar, y = input$yvar, color = "species")) +
      geom_point(size = 3, alpha = 0.8) +
      theme_minimal(base_size = 14) +
      labs(
        x = gsub("_", " ", input$xvar),
        y = gsub("_", " ", input$yvar),
        title = "Palmer Penguins Measurements",
        subtitle = paste("Species:", input$species, "| Sex:", input$sex)
      )
  })
  
  output$summaryText <- renderText({
    paste("Number of penguins displayed:", nrow(filteredData()))
  })
}

shinyApp(ui = ui, server = server)
