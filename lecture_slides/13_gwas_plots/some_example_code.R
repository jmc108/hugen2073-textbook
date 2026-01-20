library(qqman)
n <- 100000
results <- data.frame(CHR=rep(x=1:23,times=rep(n,23)))
results$P <- runif(n=length(results$CHR))
results$NLP <- -log10(results$P)
results$BP <- 1:n
results$SNP=paste0("rs",1:length(results$P))

results_s <- results[results$P < 0.01, ]
results_ns <- results[results$P >= 0.01, ]
results_ns_thinned <- results_ns[sample(1:nrow(results_ns), size=10000, replace=FALSE),]

results_thinned <- rbind(results_s, results_ns_thinned)

png("~/Desktop/manhattan_thinned.png")
manhattan(x = results_thinned)
dev.off()

qq(results$P)