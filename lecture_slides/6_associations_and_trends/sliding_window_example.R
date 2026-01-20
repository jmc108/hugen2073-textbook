# Load libraries
library(shiny)
library(data.table)
library(tidyverse)
library(zoo)  # For rollmean

# Load data
d <- fread(file="~/Downloads/SN_d_tot_V2.0.txt",
           header=FALSE,
           fill=8)
d <- d %>% select(4,5)
names(d) <- c("date","n")

# Define UI
ui <- fluidPage(
  titlePanel("Rolling Mean Smoother with Date Range"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("startYear", "Select Start Year:", 
                  min = 2000, max = 2025, value = 2000, step = 0.01, sep = ""),
      
      numericInput("window", "Window Size (k):", 
                   value = 10, min = 1, max = 1000, step = 1),
      
      selectInput("align", "Alignment:", 
                  choices = c("Center" = "center", 
                              "Left" = "left", 
                              "Right" = "right"), 
                  selected = "center")
    ),
    
    mainPanel(
      plotOutput("rollingPlot")
    )
  )
)

# Define Server
server <- function(input, output) {
  output$rollingPlot <- renderPlot({
    
    # Filter data based on selected start year (keeping 2025 fixed)
    filtered_data <- d %>% filter(date >= input$startYear & date <= 2025)
    
    # Compute rolling mean with user-selected window size & alignment
    filtered_data$rolling_avg <- rollmean(filtered_data$n, 
                                          k = input$window, 
                                          align = input$align, 
                                          fill = NA)
    
    ggplot(filtered_data, aes(x = date)) +
      geom_point(aes(y = n), alpha = 0.3, color = "black") +  # Scatterplot of raw data
      geom_line(aes(y = rolling_avg), color = "blue", size = 1.2) +  # Rolling mean line
      labs(title = "Rolling Mean with Adjustable Parameters",
           x = "Time", y = "n") +
      theme_minimal()
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
