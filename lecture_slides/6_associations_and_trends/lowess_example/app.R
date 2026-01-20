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
  titlePanel("Interactive LOESS Smoother with Date Range Filter"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("dateRange", "Select Time Range:",
                  min = 1818, max = 2025, value = c(1818, 2025), step = 1, sep = ""),
      
      sliderInput("span", "Smoothing Span:", 
                  min = 0.0001, max = 0.05, value = 0.05, step = 0.00001),
      
      selectInput("degree", "Polynomial Degree:", 
                  choices = c(1:8), selected = 1)
    ),
    
    mainPanel(
      plotOutput("loessPlot")
    )
  )
)

# Define Server
server <- function(input, output) {
  output$loessPlot <- renderPlot({
    
    # Filter data based on date range
    filtered_data <- d %>% filter(date >= input$dateRange[1] & date <= input$dateRange[2])
    
    ggplot(filtered_data, aes(x = date, y = n)) +
      geom_line(color = "black", alpha = 0.6) +  # Line graph
      geom_smooth(method = "loess", 
                  span = input$span, 
                  method.args = list(degree = as.numeric(input$degree)), 
                  color = "blue", fill = "lightblue", se=FALSE) +  # LOESS curve
      labs(title = "LOESS Smoother with Adjustable Parameters",
           x = "Time", y = "n") +
      theme_minimal()
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
