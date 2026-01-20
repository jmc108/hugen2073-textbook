# Setup
library("tidyverse")
set.seed(2076)

# Parameters
n_individuals <- 1000  # Number of individuals
n_snps <- 100000  # Number of SNPs

# Simulate genotypes (random binomial draws, assuming MAF ~ 0.5)
genotypes <- data.frame(
  matrix(rbinom(n_individuals * n_snps, 2, 0.5),
         nrow = n_individuals,
         ncol = n_snps))

# Simulate a phenotype (normally distributed, no true genetic effect)
phenotype <- rnorm(n_individuals)

# Check a random SNP and the phenotype
hist(genotypes[,1])
hist(phenotype)
ggplot() + geom_boxplot(aes(factor(genotypes[,1]), phenotype)) + 
  geom_jitter(aes(x=factor(genotypes[,1]), y=phenotype), alpha=0.3)
ggplot() +
  geom_jitter(aes(x=genotypes[,1], y=phenotype), alpha=0.3) +
  geom_smooth(aes(x=genotypes[,1], y=phenotype), method = "lm")
lm(phenotype ~ genotypes[,1])
summary(lm(phenotype ~ genotypes[,1]))
str(summary(lm(phenotype ~ genotypes[,1])))
summary(lm(phenotype ~ genotypes[,1]))$coefficients
summary(lm(phenotype ~ genotypes[,1]))$coefficients[2,4]

# Function to compute p-value
# (Repeats above process for every SNP)
compute_p_value <- function(geno, pheno) {
  model <- lm(pheno ~ geno)
  summary(model)$coefficients[2, 4]  # Extract p-value for genotype effect
}

# Compute p-values for all SNPs
p_values <- apply(genotypes, 2, compute_p_value, pheno = phenotype)

# Plot histogram of p-values
ggplot(data.frame(p_values), aes(x = p_values)) +
  geom_histogram(bins = 50, fill = "blue", alpha = 0.5, color = "black") +
  labs(title = "Histogram of p-values under the Null Hypothesis",
       x = "p-value",
       y = "Frequency") +
  theme_minimal()

# Alternatively, plot CDF of p-values
plot(ecdf(p_values))
lines(x=c(0,1),y=c(0,1), col="red", lwd=2, lty=2)

# QQ plot to compare to uniform distribution
expected_pvals <- (1:n_snps) / n_snps
qqplot(-log10(expected_pvals), -log10(sort(p_values)),
       xlab = "Expected -log10(p)", ylab = "Observed -log10(p)", main = "QQ Plot of p-values")
abline(0, 1, col = "red")
