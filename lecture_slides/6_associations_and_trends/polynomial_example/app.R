# Load libraries
library("shiny")
library("data.table")
library("tidyverse")

# Get data
d <- fread(file="~/Downloads/SN_d_tot_V2.0.txt",
           header=FALSE,
           fill=8)
d <- d %>% select(4,5)
names(d) <- c("date","n")

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Polynomial Regression with Date Filter"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("lowerDate", "Select Start Year:",
                  min = 2010, max = 2025, value = 2010, step = 0.01, sep = ""),
      
      sliderInput("degree", "Polynomial Degree:", 
                  min = 1, max = 30, value = 2, step = 1)
    ),
    
    mainPanel(
      plotOutput("polyPlot")
    )
  )
)

# Define Server
server <- function(input, output) {
  output$polyPlot <- renderPlot({
    
    # Filter data from the selected lower date to the fixed upper date (2025)
    filtered_data <- d %>% filter(date >= input$lowerDate & date <= 2025)
    
    ggplot(filtered_data, aes(x = date, y = n)) +
      geom_point(alpha = 0.5) +  # Scatter plot
      geom_smooth(method = "lm", formula = y ~ poly(x, input$degree), color = "red") +  # Polynomial fit
      labs(title = paste(input$degree, "Degree Polynomial Fit"),
           x = "Time", y = "n") +
      theme_minimal()
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
