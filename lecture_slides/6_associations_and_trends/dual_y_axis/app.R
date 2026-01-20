library(shiny)
library(ggplot2)

# Simulate dataset
set.seed(123)
n <- 100
data <- data.frame(
  x = 1:n,
  y1 = cumsum(rnorm(n, mean = 0.5, sd = 1)) * 50,  # Scaled trend
  y2 = cumsum(rnorm(n, mean = -0.3, sd = 1)) * 50  # Opposite trend
)

ui <- fluidPage(
  titlePanel("Dual-Axis Line Graph with Adjustable Ranges"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("y1_min", "Y1 Axis Min:", min = -10000, max = 10000, value = min(data$y1), step = 10),
      sliderInput("y1_max", "Y1 Axis Max:", min = -10000, max = 10000, value = max(data$y1), step = 10),
      
      sliderInput("y2_min", "Y2 Axis Min:", min = -10000, max = 10000, value = min(data$y2), step = 10),
      sliderInput("y2_max", "Y2 Axis Max:", min = -10000, max = 10000, value = max(data$y2), step = 10)
    ),
    
    mainPanel(
      plotOutput("linePlot")
    )
  )
)

server <- function(input, output) {
  output$linePlot <- renderPlot({
    # Rescale y2 dynamically to fit within the selected range
    y2_scaled <- scales::rescale(data$y2, to = c(input$y1_min, input$y1_max), from = c(input$y2_min, input$y2_max))
    
    ggplot(data, aes(x = x)) +
      geom_line(aes(y = y1, color = "Y1")) +
      geom_line(aes(y = y2_scaled, color = "Y2")) + 
      scale_y_continuous(
        name = "Y1 Axis",
        limits = c(input$y1_min, input$y1_max),
        sec.axis = sec_axis(
          trans = ~ scales::rescale(., from = c(input$y1_min, input$y1_max), to = c(input$y2_min, input$y2_max)),
          name = "Y2 Axis"
        )
      ) +
      scale_color_manual(values = c("Y1" = "blue", "Y2" = "red")) +
      theme_minimal() +
      theme(
        legend.title = element_blank(),
        axis.title.y.left = element_text(color = "blue"),
        axis.title.y.right = element_text(color = "red")
      )
  })
}

shinyApp(ui, server)
