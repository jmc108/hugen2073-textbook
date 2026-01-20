# Load necessary library
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Number of individuals and SNPs
n_individuals <- 5000  # Number of individuals
n_snps <- 10000  # Total number of SNPs
n_causal <- 50  # Number of true causal SNPs

# Simulate genotype matrix (random binomial draws, assuming MAF ~ 0.5)
genotypes <- matrix(rbinom(n_individuals * n_snps, 2, 0.5), nrow = n_individuals, ncol = n_snps)

# Assign effect sizes: 
# - 500 SNPs have a true effect, drawn from a normal distribution
# - The rest have no effect
effect_sizes <- rep(0, n_snps)  # Default effect size = 0
causal_indices <- sample(1:n_snps, n_causal)  # Randomly select causal SNPs
effect_sizes[causal_indices] <- rnorm(n_causal, mean = 0, sd = 1)  # Small effect sizes

# Simulate phenotype as a function of causal SNPs + noise
genetic_contribution <- genotypes[, causal_indices] %*% effect_sizes[causal_indices]
phenotype <- genetic_contribution + rnorm(n_individuals, sd = 10)  # Add random noise

# Function to compute p-value for association (linear regression)
compute_p_value <- function(geno, pheno) {
  model <- lm(pheno ~ geno)
  summary(model)$coefficients[2, 4]  # Extract p-value for genotype effect
}

# Compute p-values for all SNPs
p_values <- apply(genotypes, 2, compute_p_value, pheno = phenotype)

# Plot histogram of p-values
ggplot(data.frame(p_values), aes(x = p_values,    fill=(1:length(p_values) %in% causal_indices)        )) +
  geom_histogram(bins = 50, alpha = 0.5, color = "black") +
  labs(title = "Histogram of p-values with 500 Causal SNPs",
       x = "p-value",
       y = "Frequency") +
  theme_minimal()

# Alternatively, plot CDF of p-values
plot(ecdf(p_values))
lines(x=c(0,1),y=c(0,1), col="red", lwd=2, lty=2)

# QQ plot to compare observed vs. expected p-values
expected_pvals <- (1:n_snps) / n_snps
qqplot(-log10(expected_pvals), -log10(sort(p_values)),
       xlab = "Expected -log10(p)", ylab = "Observed -log10(p)", main = "QQ Plot of p-values")
abline(0, 1, col = "red")
