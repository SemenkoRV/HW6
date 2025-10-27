# app.R
library(shiny)

ui <- fluidPage(
  
  # App title
  titlePanel("Demo of Five Shiny Widgets"),
  
  sidebarLayout(
    sidebarPanel(
      # 1. Slider Input: Number of points
      sliderInput(
        inputId = "n_points",
        label = "Number of Points:",
        min = 10,
        max = 1000,
        value = 100,
        step = 10
      ),
      
      # 2. Select Input: Distribution type
      selectInput(
        inputId = "dist_type",
        label = "Distribution Type:",
        choices = c("Normal" = "norm", "Uniform" = "unif", "Exponential" = "exp"),
        selected = "norm"
      ),
      
      # 3. Checkbox Input: Show density curve
      checkboxInput(
        inputId = "show_density",
        label = "Show Density Curve",
        value = TRUE
      ),
      
      # 4. Radio Buttons: Plot color
      radioButtons(
        inputId = "color_choice",
        label = "Plot Color:",
        choices = c("Blue" = "blue", "Red" = "red", "Green" = "green"),
        selected = "blue"
      ),
      
      # 5. Text Input: Plot title
      textInput(
        inputId = "plot_title",
        label = "Plot Title:",
        value = "My Random Distribution"
      )
    ),
    
    mainPanel(
      plotOutput(outputId = "distPlot")
    )
  )
)

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # Generate random data based on user input
    data <- switch(input$dist_type,
                   norm = rnorm(input$n_points),
                   unif = runif(input$n_points),
                   exp = rexp(input$n_points))
    
    # Plot the data
    hist(data, 
         col = input$color_choice,
         main = input$plot_title,
         border = "white",
         probability = input$show_density)
    
    # Optionally add a density curve
    if (input$show_density) {
      lines(density(data), col = "black", lwd = 2)
    }
  })
}

shinyApp(ui = ui, server = server)
