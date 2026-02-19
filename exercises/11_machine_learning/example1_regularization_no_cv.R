# Golub ALL vs AML: Logistic vs Ridge vs Lasso vs Elastic Net
# NO train/test split, NO CV

#BiocManager::install("golubEsets")
#install.packages("glmnet")

library(golubEsets)
library(glmnet)

# Data
data(Golub_Merge)

X <- t(exprs(Golub_Merge))  # samples x genes
y <- factor(pData(Golub_Merge)$ALL.AML)  # "ALL" vs "AML"
y_bin <- ifelse(y == "AML", 1, 0)        # AML=1, ALL=0 (arbitrary)

gene_ids <- featureNames(Golub_Merge)    # probe ids / gene identifiers
n <- nrow(X); p <- ncol(X)
cat("n =", n, "p =", p, "\n")

# Fixed lambda (for example)
lambda_val <- 0.1

# Fit models
# (A) Unpenalized logistic regression via glm()
# This might fail!
logit_fit <- tryCatch(
  glm(y_bin ~ ., data = data.frame(y_bin = y_bin, X), family = binomial),
  error = function(e) e
)

# (B) glmnet fits
ridge_fit <- glmnet(X, y_bin, family = "binomial", alpha = 0,   lambda = lambda_val)
lasso_fit <- glmnet(X, y_bin, family = "binomial", alpha = 1,   lambda = lambda_val)
en_fit    <- glmnet(X, y_bin, family = "binomial", alpha = 0.5, lambda = lambda_val)

# extract betas (exclude intercept)
get_beta_glmnet <- function(fit) {
  b <- as.vector(coef(fit))[-1]
  b
}

b_ridge <- get_beta_glmnet(ridge_fit)
b_lasso <- get_beta_glmnet(lasso_fit)
b_en    <- get_beta_glmnet(en_fit)

# logistic betas (if model fit succeeded)
b_logit <- rep(NA_real_, p)
logit_ok <- !inherits(logit_fit, "error")
if (logit_ok) {
  # coef(logit_fit) includes intercept then coefficients for columns (if estimable)
  # When p >> n, glm may drop/alias predictors; we align by column names if possible.
  # Column names in data.frame(y_bin, X) are "y_bin" then colnames(X)
  cf <- coef(logit_fit)
  cf <- cf[names(cf) != "(Intercept)"]
  b_logit <- rep(0, p)
  names(b_logit) <- colnames(X)
  overlap <- intersect(names(b_logit), names(cf))
  b_logit[overlap] <- cf[overlap]
} else {
  message("Unpenalized logistic failed to fit (expected in p >> n).")
}

# 2. Plot: # nonzero coefficients (sparsity)

nnz <- function(b, tol = 1e-10) sum(abs(b) > tol)

nz_counts <- c(
  Logistic = if (logit_ok) nnz(b_logit) else NA_real_,
  Ridge    = nnz(b_ridge),
  Lasso    = nnz(b_lasso),
  EN       = nnz(b_en)
)

par(mfrow = c(1,1))
barplot(nz_counts,
        main = paste0("Sparsity at lambda = ", lambda_val),
        ylab = "# nonzero coefficients (genes)",
        las = 1)

# 3. Plot: distribution of |beta|
#    (ridge vs lasso vs EN; logistic optional)

abs_list <- list(
  Ridge = abs(b_ridge),
  Lasso = abs(b_lasso),
  EN    = abs(b_en)
)

# Optionally add logistic if it fit and is meaningful
if (logit_ok) abs_list$Logistic <- abs(b_logit)

# Use a common x-range (drop infinities / NAs)
all_abs <- unlist(abs_list)
all_abs <- all_abs[is.finite(all_abs)]
xmax <- quantile(all_abs, 0.99, na.rm = TRUE)

# density plots (excluding exact zeros helps show shapes; we'll include a zero-spike separately)
dens <- lapply(abs_list, function(v) density(v[v > 0 & is.finite(v)], na.rm = TRUE))

plot(dens[[1]],
     main = "|β| distributions (excluding exact zeros)",
     xlab = "|β|", xlim = c(0, xmax))
if (length(dens) > 1) {
  for (k in 2:length(dens)) lines(dens[[k]])
}
legend("topright", legend = names(dens), lwd = 1, bty = "n")

# Show the zero-mass explicitly (a simple barplot)
zero_frac <- sapply(abs_list, function(v) mean(abs(v) <= 1e-10, na.rm = TRUE))
barplot(zero_frac,
        main = "Fraction of coefficients exactly 0",
        ylab = "fraction",
        las = 1)


# Training Accuracy for All Models

get_train_acc_glmnet <- function(fit, X, y) {
  prob <- as.vector(predict(fit, X, type = "response"))
  pred <- ifelse(prob > 0.5, 1, 0)
  mean(pred == y)
}

# Logistic regression accuracy
if (logit_ok) {
  logit_prob <- predict(logit_fit, type = "response")
  logit_pred <- ifelse(logit_prob > 0.5, 1, 0)
  acc_logit  <- mean(logit_pred == y_bin)
} else {
  acc_logit <- NA
}

# Ridge / Lasso / EN accuracy
acc_ridge <- get_train_acc_glmnet(ridge_fit, X, y_bin)
acc_lasso <- get_train_acc_glmnet(lasso_fit, X, y_bin)
acc_en    <- get_train_acc_glmnet(en_fit, X, y_bin)

# Collect
acc_vec <- c(
  Logistic = acc_logit,
  Ridge    = acc_ridge,
  Lasso    = acc_lasso,
  EN       = acc_en
)

print(acc_vec)

# Bar plot
barplot(acc_vec,
        main = "Training Accuracy (No Train/Test Split)",
        ylab = "Accuracy",
        ylim = c(0, 1),
        las = 1)
