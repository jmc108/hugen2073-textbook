library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hungry"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      textInput(inputId = "text_entry",
                label = "Enter some text in the widget. Watch the output update:",
                value = "default text"),
      
      sliderInput(inputId = "hungry",
                  label = "This is how hungry I am right now:",
                  min = 1,
                  max = 10,
                  value = 4)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      textOutput(outputId = "text_entry"),
      textOutput(outputId = "hungry_message")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Restating the uder-given text ----
  # with requested number of bins
  # This expression that generates text is wrapped in a call
  # to renderText to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$text_entry) change
  # 2. Its output type is text
  output$text_entry <- renderText({
    
    paste0("You entered: ", input$text_entry)
    
  })
  
  output$hungry_message <- renderText({
    
    paste0("You said your hunger was ", input$hungry, " on a scale of 1-10")
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)