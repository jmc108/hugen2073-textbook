# Load packages
library("data.table")
library("shiny")
library("tidyverse")
library("ggplot2")
library("ggtext")

# Read in data
d <- fread("/Users/jonathanchernus/Documents/Teaching/2023s/hugen2073/lecture_slides/16_shiny/16_shiny_data/chip_2_genesis_gwas_results.txt.gz")
d$chr <- as.numeric(d$chr)


# Define UI
ui <- fluidPage(
  
  # App title
  titlePanel("Manhattan plot"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Input: Slider for genome-wide significance
      sliderInput(inputId = "sig",
                  label = "Genome-wide significance threshold (-logp scale):",
                  min = 5,
                  max = 200,
                  value=7.3),
    
      # Input: Slider for P-value filtering threshold
      sliderInput(inputId = "p_thresh_filter",
                  label = "P-value filtering threshold:",
                  min = 0,
                  max = 0.1,
                  value=0.05),
      # Input: Slider for P-value filtering fraction
    sliderInput(inputId = "p_thresh_fraction",
                label = "P-value filtering fraction:",
                min = 0,
                max = 1,
                value=0.1),
    
    # Input: Slider for y-axis limits
    sliderInput(inputId = "ymax",
                label = "Y-axis upper limit:",
                min = 0,
                max = 100,
                value=20)),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "manhplot")
      
    )
  )
)











# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$manhplot <- renderPlot({
    
    
    
    
    # Set thresholds for filtering
    #p_thresh_filter <- 0.05
    #p_thresh_fraction <- 0.1
    
    # Set threshold for genome-wide significance
    #sig <- 5e-8
    
    ##########
    ##########
    ##########
    
    # Split data into significant and non-significant parts
    # This will help speed up plotting
    sig_data <- d %>% 
      subset(Score.pval < input$p_thresh_filter)
    notsig_data <- d %>% 
      subset(Score.pval >= input$p_thresh_filter) %>%
      group_by(chr) %>% 
      sample_frac(input$p_thresh_fraction)
    gwas_data <- bind_rows(sig_data, notsig_data)
    
    
    # Change plotting character for y > ymax
    # For points with y > ymax, change the pvalue
    gwas_data$ybig <- -log10(gwas_data$Score.pval) > input$ymax
    gwas_data$p_ymax_cutoff <- gwas_data$Score.pval
    gwas_data$p_ymax_cutoff[  -log10(gwas_data$p_ymax_cutoff) > input$ymax ] <- 10^(-input$ymax)
    
    # Preparing the x-axis
    data_cum <- gwas_data %>% 
      group_by(chr) %>% 
      summarise(max_bp = max(pos)) %>% 
      mutate(bp_add = lag(cumsum(as.numeric(max_bp)), default = 0)) %>% 
      select(chr, bp_add)
    
    gwas_data <- gwas_data %>% 
      inner_join(data_cum, by = "chr") %>% 
      mutate(bp_cum = pos + bp_add)
    
    
    # Preparing the x-axis labels and the y-axis
    axis_set <- gwas_data %>% 
      group_by(chr) %>% 
      summarize(center = mean(bp_cum))
    
    ylim <- gwas_data %>% 
      filter(Score.pval == min(Score.pval)) %>% 
      mutate(ylim = abs(floor(log10(Score.pval))) + 2) %>% 
      pull(ylim)
    
    # Create the plot object
    manhplot <- ggplot(gwas_data,
                       aes(x = bp_cum,
                           y = -log10(p_ymax_cutoff),
                           color = as_factor(chr),
                           shape = ybig,
                           size = -log10(Score.pval))) +
      # Genome-wide significance line
      geom_hline(yintercept = input$sig, color = "grey40", linetype = "dashed") + 
      # Make point a little transparent
      geom_point(alpha = 0.75) +
      # Set x-axis labels
      scale_x_continuous(label = axis_set$chr, breaks = axis_set$center) +
      # Set y-axis limits
      scale_y_continuous(expand = c(0,0), limits = c(0, input$ymax + 1)) +
      # Set point color (alternate by chromosome)
      scale_color_manual(values = rep(c("#276FBF", "#183059"), unique(length(axis_set$chr)))) +
      # Prevent plotted points from being too big or too small
      scale_size_continuous(range = c(0.5,3)) +
      # Axis names
      labs(x = NULL, 
           y = "-log<sub>10</sub>(p)") + 
      # Theme
      theme_minimal() +
      # Legend/axis tick label adjustments
      theme( 
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_markdown(),
        axis.text.x = element_text(angle = 0, size = 8, vjust = 0.5)
      )
    
    
    
    print(manhplot)
    
    
    
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)