library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("How reactivity works"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      textInput(inputId = "text_entry",
                  label = "Enter some text in the widget. Watch the output update:",
                  value = "default text")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      textOutput(outputId = "text_entry")
      
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
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)