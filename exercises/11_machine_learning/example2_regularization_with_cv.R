# =========================================================
# Golub ALL vs AML: Logistic vs Ridge vs Lasso vs Elastic Net
# FULL PIPELINE IN ONE BLOCK:
#  - Train/Test split (holdout test set)
#  - CV on TRAINING set to choose lambda (and fixed alpha for EN)
#  - Fit final models on TRAINING set at CV-chosen lambda
#  - Keep (nearly) all previous plots:
#      * CV curves with lambda.min / lambda.1se marked
#      * Sparsity (# nonzero coefficients)
#      * |beta| distributions + fraction exactly zero
#      * Top genes by |beta|
#      * Coefficient path plots
#  - Add FINAL TEST performance:
#      * Bar plots: Accuracy + AUC
#      * Confusion matrices (heatmaps via base image)
#
# Notes:
#  - Elastic net has 2 hyperparameters (alpha, lambda).
#    Here we FIX alpha=0.5 and CV only lambda (standard cv.glmnet usage).
#  - Logistic glm() in p >> n may warn/fail; we keep it as a teaching contrast.
# =========================================================

# Packages

library(golubEsets)
library(glmnet)
library(pROC)

# Data
data(Golub_Merge)

X <- t(exprs(Golub_Merge))                 # samples x genes
y <- factor(pData(Golub_Merge)$ALL.AML)    # "ALL" vs "AML"
y_bin <- ifelse(y == "AML", 1, 0)          # AML=1, ALL=0

gene_ids <- featureNames(Golub_Merge)
n <- nrow(X); p <- ncol(X)
cat("n =", n, "p =", p, "\n")

# 0.  Train/Test split
set.seed(1)
train_frac <- 0.7
train_idx <- sample(seq_len(n), size = floor(train_frac * n))
test_idx  <- setdiff(seq_len(n), train_idx)

X_train <- X[train_idx, , drop = FALSE]
X_test  <- X[test_idx,  , drop = FALSE]
y_train <- y_bin[train_idx]
y_test  <- y_bin[test_idx]

cat("Train n =", nrow(X_train), " Test n =", nrow(X_test), "\n")

# 1. Fit models (CV on TRAIN only for ridge/lasso/EN)

# (A) Unpenalized logistic regression via glm() on TRAIN
# (expected to be unstable when p >> n)
logit_fit <- tryCatch(
  glm(y_train ~ ., data = data.frame(y_train = y_train, X_train), family = binomial),
  error = function(e) e
)
logit_ok <- !inherits(logit_fit, "error")

# (B) CV fits for ridge/lasso/EN (choose lambda on TRAIN only)
k_folds <- 10
type_measure <- "deviance"  # for binomial

set.seed(1)
cv_ridge <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 0,
                      nfolds = k_folds, type.measure = type_measure)

set.seed(1)
cv_lasso <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 1,
                      nfolds = k_folds, type.measure = type_measure)

alpha_en <- 0.5
set.seed(1)
cv_en <- cv.glmnet(X_train, y_train, family = "binomial", alpha = alpha_en,
                   nfolds = k_folds, type.measure = type_measure)

# Choose lambda rule (min vs 1se)
use_1se <- FALSE
pick_lambda <- function(cvobj) if (use_1se) cvobj$lambda.1se else cvobj$lambda.min

lam_ridge <- pick_lambda(cv_ridge)
lam_lasso <- pick_lambda(cv_lasso)
lam_en    <- pick_lambda(cv_en)

cat("\nChosen lambdas (", if (use_1se) "lambda.1se" else "lambda.min", "):\n", sep = "")
print(data.frame(
  Model = c("Ridge", "Lasso", paste0("EN (alpha=", alpha_en, ")")),
  lambda = c(lam_ridge, lam_lasso, lam_en)
))

# Fit final penalized models on TRAIN at chosen lambda
ridge_fit <- glmnet(X_train, y_train, family = "binomial", alpha = 0,   lambda = lam_ridge)
lasso_fit <- glmnet(X_train, y_train, family = "binomial", alpha = 1,   lambda = lam_lasso)
en_fit    <- glmnet(X_train, y_train, family = "binomial", alpha = alpha_en, lambda = lam_en)

# helpers
get_beta_glmnet <- function(fit) as.vector(coef(fit))[-1]  # exclude intercept
nnz <- function(b, tol = 1e-10) sum(abs(b) > tol)

# For logistic glm() coefficients (align to gene columns)
get_beta_logit_aligned <- function(fit, p, colnamesX) {
  b <- rep(0, p); names(b) <- colnamesX
  cf <- coef(fit)
  cf <- cf[names(cf) != "(Intercept)"]
  overlap <- intersect(names(b), names(cf))
  b[overlap] <- cf[overlap]
  b
}

# Accuracy/AUC from predicted probabilities
metrics_from_prob <- function(prob, y_true) {
  pred <- ifelse(prob > 0.5, 1, 0)
  acc <- mean(pred == y_true)
  aucv <- as.numeric(pROC::auc(y_true, prob))
  list(acc = acc, auc = aucv, pred = pred)
}

# Confusion matrix (counts)
conf_mat <- function(y_true, y_pred) {
  # rows=true, cols=pred; order 0 then 1
  tab <- table(factor(y_true, levels = c(0,1)),
               factor(y_pred, levels = c(0,1)))
  dimnames(tab) <- list(True = c("0","1"), Pred = c("0","1"))
  tab
}

# Heatmap-ish plot for confusion matrix
plot_confmat <- function(cm, main = "Confusion matrix") {
  m <- as.matrix(cm)
  # image() needs numeric matrix; flip vertically for nicer orientation
  image(t(m[nrow(m):1, , drop=FALSE]),
        axes = FALSE, main = main)
  axis(1, at = seq(0,1,length.out=ncol(m)), labels = colnames(m))
  axis(2, at = seq(0,1,length.out=nrow(m)), labels = rev(rownames(m)))
  # overlay counts
  for (i in 1:nrow(m)) for (j in 1:ncol(m)) {
    # map (i,j) to image coordinates: j along x, reversed i along y
    x <- (j-1)/(ncol(m)-1)
    y <- (nrow(m)-i)/(nrow(m)-1)
    text(x, y, labels = m[i,j])
  }
  box()
}

# 2. CV curve plots (TRAIN CV only) with best lambdas marked
par(mfrow = c(1,3))

plot(cv_ridge, main = paste0("Ridge CV (k=", k_folds, ", train only)"))
abline(v = log(cv_ridge$lambda.min), lty = 2)
abline(v = log(cv_ridge$lambda.1se), lty = 3)
legend("topright", legend = c("lambda.min", "lambda.1se"), lty = c(2,3), bty="n")

plot(cv_lasso, main = paste0("Lasso CV (k=", k_folds, ", train only)"))
abline(v = log(cv_lasso$lambda.min), lty = 2)
abline(v = log(cv_lasso$lambda.1se), lty = 3)
legend("topright", legend = c("lambda.min", "lambda.1se"), lty = c(2,3), bty="n")

plot(cv_en, main = paste0("EN CV (alpha=", alpha_en, ", train only)"))
abline(v = log(cv_en$lambda.min), lty = 2)
abline(v = log(cv_en$lambda.1se), lty = 3)
legend("topright", legend = c("lambda.min", "lambda.1se"), lty = c(2,3), bty="n")

par(mfrow = c(1,1))

# 3. Betas at chosen lambda (penalized models) + logistic (train)
b_ridge <- get_beta_glmnet(ridge_fit)
b_lasso <- get_beta_glmnet(lasso_fit)
b_en    <- get_beta_glmnet(en_fit)

b_logit <- rep(NA_real_, p)
if (logit_ok) b_logit <- get_beta_logit_aligned(logit_fit, p, colnames(X_train))

# 4. Sparsity plot (# nonzero)
nz_counts <- c(
  Logistic = if (logit_ok) nnz(b_logit) else NA_real_,
  Ridge    = nnz(b_ridge),
  Lasso    = nnz(b_lasso),
  EN       = nnz(b_en)
)

barplot(nz_counts,
        main = paste0("Sparsity at CV-chosen lambda (", if (use_1se) "1se" else "min", ")"),
        ylab = "# nonzero coefficients (genes)",
        las = 1)

# 5. |beta| distributions + fraction exactly zero
abs_list <- list(
  Ridge = abs(b_ridge),
  Lasso = abs(b_lasso),
  EN    = abs(b_en)
)
if (logit_ok) abs_list$Logistic <- abs(b_logit)

all_abs <- unlist(abs_list)
all_abs <- all_abs[is.finite(all_abs)]
xmax <- quantile(all_abs, 0.99, na.rm = TRUE)

dens <- lapply(abs_list, function(v) {
  v_clean <- v[v > 0 & is.finite(v)]
  if (length(v_clean) > 1) {
    density(v_clean)
  } else {
    NULL
  }
})
dens_nonnull <- dens[!sapply(dens, is.null)]


if (length(dens_nonnull) > 0) {
  plot(dens_nonnull[[1]],
       main = "|β| distributions (excluding exact zeros)",
       xlab = "|β|")
  
  if (length(dens_nonnull) > 1) {
    for (k in 2:length(dens_nonnull)) {
      lines(dens_nonnull[[k]])
    }
  }
  
  legend("topright",
         legend = names(dens_nonnull),
         lwd = 1,
         bty = "n")
} else {
  plot.new()
  title("No nonzero coefficients to plot")
}
zero_frac <- sapply(abs_list, function(v) mean(abs(v) <= 1e-10, na.rm = TRUE))
barplot(zero_frac,
        main = "Fraction of coefficients exactly 0",
        ylab = "fraction",
        las = 1)



# 7. Coefficient path plots (from TRAIN fits used by CV)
ridge_path <- cv_ridge$glmnet.fit
lasso_path <- cv_lasso$glmnet.fit
en_path    <- cv_en$glmnet.fit

par(mfrow = c(1,3))
plot(ridge_path, xvar = "lambda", main = "Ridge coefficient paths")
plot(lasso_path, xvar = "lambda", main = "Lasso coefficient paths")
plot(en_path,    xvar = "lambda", main = paste0("EN paths (alpha=", alpha_en, ")"))
par(mfrow = c(1,1))

# 8. FINAL HOLDOUT TEST PERFORMANCE (Accuracy + AUC + Confusion Matrices)

# Predict probabilities on TEST
# Penalized models
prob_ridge <- as.vector(predict(ridge_fit, X_test, type = "response"))
prob_lasso <- as.vector(predict(lasso_fit, X_test, type = "response"))
prob_en    <- as.vector(predict(en_fit,    X_test, type = "response"))

# Unpenalized logistic (if fit succeeded)
if (logit_ok) {
  # predict() for glm uses same feature columns it was trained with
  prob_logit <- tryCatch(
    as.vector(predict(logit_fit, newdata = data.frame(X_train = X_test), type = "response")),
    error = function(e) rep(NA_real_, length(y_test))
  )
} else {
  prob_logit <- rep(NA_real_, length(y_test))
}

# Compute metrics
m_ridge <- metrics_from_prob(prob_ridge, y_test)
m_lasso <- metrics_from_prob(prob_lasso, y_test)
m_en    <- metrics_from_prob(prob_en,    y_test)

if (all(is.finite(prob_logit))) {
  m_logit <- metrics_from_prob(prob_logit, y_test)
} else {
  m_logit <- list(acc = NA_real_, auc = NA_real_, pred = rep(NA_integer_, length(y_test)))
}

acc_vec_test <- c(
  Logistic = m_logit$acc,
  Ridge    = m_ridge$acc,
  Lasso    = m_lasso$acc,
  EN       = m_en$acc
)
auc_vec_test <- c(
  Logistic = m_logit$auc,
  Ridge    = m_ridge$auc,
  Lasso    = m_lasso$auc,
  EN       = m_en$auc
)

print(data.frame(Model = names(acc_vec_test),
                 Test_Accuracy = acc_vec_test,
                 Test_AUC = auc_vec_test))

par(mfrow = c(1,2))
barplot(acc_vec_test,
        main = "Holdout Test Accuracy",
        ylab = "Accuracy", ylim = c(0,1), las = 1)

barplot(auc_vec_test,
        main = "Holdout Test AUC",
        ylab = "AUC", ylim = c(0,1), las = 1)
par(mfrow = c(1,1))

# Confusion matrices (TEST)
cm_logit <- if (logit_ok && all(is.finite(prob_logit))) conf_mat(y_test, m_logit$pred) else NA
cm_ridge <- conf_mat(y_test, m_ridge$pred)
cm_lasso <- conf_mat(y_test, m_lasso$pred)
cm_en    <- conf_mat(y_test, m_en$pred)

par(mfrow = c(2,2))
if (is.matrix(cm_logit) || is.table(cm_logit)) {
  plot_confmat(cm_logit, main = "Logistic (test)")
} else {
  plot.new(); title("Logistic (test)\nfit failed / NA")
}
plot_confmat(cm_ridge, main = "Ridge (test)")
plot_confmat(cm_lasso, main = "Lasso (test)")
plot_confmat(cm_en,    main = "Elastic Net (test)")
par(mfrow = c(1,1))

# ---- OPTIONAL: If you want to tune alpha too (outer loop over alpha, inner CV for lambda) ----
# alphas <- c(0.1, 0.3, 0.5, 0.7, 0.9)
# set.seed(1)
# cv_list <- lapply(alphas, function(a) cv.glmnet(X_train, y_train, family="binomial", alpha=a, nfolds=k_folds))
# cv_mins <- sapply(cv_list, function(cv) min(cv$cvm))
# best_i <- which.min(cv_mins)
# best_alpha <- alphas[best_i]
# best_cv <- cv_list[[best_i]]
# cat("\nBest EN alpha by CV:", best_alpha, "\n")
# cat("Best EN lambda.min:", best_cv$lambda.min, "\n")
# plot(best_cv, main=paste0("Best EN CV (alpha=", best_alpha, ")"))
# abline(v=log(best_cv$lambda.min), lty=2)
# abline(v=log(best_cv$lambda.1se), lty=3)


# ROC Curves (Holdout Test Set)

# Create ROC objects (using test-set probabilities)
roc_ridge <- roc(y_test, prob_ridge, quiet = TRUE)
roc_lasso <- roc(y_test, prob_lasso, quiet = TRUE)
roc_en    <- roc(y_test, prob_en,    quiet = TRUE)

# Logistic may have failed; guard it
if (logit_ok && all(is.finite(prob_logit))) {
  roc_logit <- roc(y_test, prob_logit, quiet = TRUE)
}

# Plot
plot(roc_ridge, col = "blue",  lwd = 2,
     main = "ROC Curves (Holdout Test Set)")

lines(roc_lasso, col = "red",   lwd = 2)
lines(roc_en,    col = "darkgreen", lwd = 2)

if (exists("roc_logit")) {
  lines(roc_logit, col = "black", lwd = 2, lty = 2)
}

legend("bottomright",
       legend = c(
         paste0("Ridge (AUC=", round(auc(roc_ridge),3), ")"),
         paste0("Lasso (AUC=", round(auc(roc_lasso),3), ")"),
         paste0("EN (AUC=", round(auc(roc_en),3), ")"),
         if (exists("roc_logit"))
           paste0("Logistic (AUC=", round(auc(roc_logit),3), ")")
       ),
       col = c("blue","red","darkgreen","black"),
       lwd = 2,
       lty = c(1,1,1,2),
       bty = "n")
