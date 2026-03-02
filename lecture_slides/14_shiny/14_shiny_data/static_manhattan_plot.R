# Load packages
library("data.table")
library("shiny")
library("tidyverse")
library("ggplot2")
library("ggtext")

# Read in data
d <- fread("/Users/jonathanchernus/Documents/Teaching/2023s/hugen2073/lecture_slides/16_shiny/16_shiny_data/chip_2_genesis_gwas_results.txt.gz")
d$chr <- as.numeric(d$chr)

# Set thresholds for filtering
p_thresh_filter <- 0.05
p_thresh_fraction <- 0.1

# Set threshold for genome-wide significance
sig <- 5e-8

##########
##########
##########

# Split data into "significant" and "non-significant" parts
# SNPs with p > p_thresh_filter will get filtered,
#   and only p_thresh_fraction of them will get shown
# This will help speed up plotting
sig_data <- d %>% 
  subset(Score.pval < p_thresh_filter)
notsig_data <- d %>% 
  subset(Score.pval >= p_thresh_filter) %>%
  group_by(chr) %>% 
  sample_frac(p_thresh_fraction)
gwas_data <- bind_rows(sig_data, notsig_data)

# Preparing the x-axis values
# This block of code just creates a modified version of the base-pair position,
#   so that the SNPs can be plotted on a common x-axis
data_cum <- gwas_data %>% 
  group_by(chr) %>% 
  summarise(max_bp = max(pos)) %>% 
  mutate(bp_add = lag(cumsum(as.numeric(max_bp)), default = 0)) %>% 
  select(chr, bp_add)
gwas_data <- gwas_data %>% 
  inner_join(data_cum, by = "chr") %>% 
  mutate(bp_cum = pos + bp_add)

# Preparing the x-axis labels and the y-axis
# x-axis labels will go in the middle of each chromosome
# y-axis upper limit will be just a little above the top SNP
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
                       y = -log10(Score.pval),
                       color = as_factor(chr),
                       size = -log10(Score.pval))) +
  # Genome-wide significance line
  geom_hline(yintercept = -log10(sig), color = "grey40", linetype = "dashed") + 
  # Make point a little transparent
  geom_point(alpha = 0.75) +
  # Set x-axis labels
  scale_x_continuous(label = axis_set$chr, breaks = axis_set$center) +
  # Set y-axis limits
  scale_y_continuous(expand = c(0,0), limits = c(0, ylim)) +
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
    axis.text.x = element_text(angle = 0, size = 5, vjust = 0.5)
  )

# Display plot
print(manhplot)