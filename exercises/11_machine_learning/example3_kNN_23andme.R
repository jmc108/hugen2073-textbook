# --- Project a 23andMe sample into HapMap (SNPRelate example GDS) PCA via SVD (ONE script) ---

library(SNPRelate)
library(data.table)
library(ggplot2)

# -----------------------
# 0) Inputs
# -----------------------
jon_path <- "/Users/jonathanchernus/Documents/Personal/medical/phased_genotype_data_Jon_Chernus/phased_genome_Jon_Chernus_v4_Full_20250403180654.txt"

# -----------------------
# 1) Read 23andMe-like file
# Expect columns: "# rsid" (or "rsid"), chromosome, position, allele1, allele2
# -----------------------
jon <- fread(jon_path)

# If the rsid column name differs, normalize it:
if (!("# rsid" %in% names(jon)) && ("rsid" %in% names(jon))) {
  setnames(jon, "rsid", "# rsid")
}
stopifnot("# rsid" %in% names(jon))
stopifnot(all(c("allele1","allele2") %in% names(jon)))

# -----------------------
# 2) Open HapMap example GDS
# -----------------------
showfile.gds(closeall = TRUE)
genofile <- snpgdsOpen(snpgdsExampleFileName())

# -----------------------
# 3) Reference LD pruning
# -----------------------
set.seed(1)
snpset <- snpgdsLDpruning(genofile, ld.threshold = 0.2)
snpset.id <- unlist(snpset, use.names = FALSE)

# -----------------------
# 4) Reference PCA scores (for plotting) using SNPRelate
# (We will compute projection using SVD later on the intersected SNP set.)
# -----------------------
pca_ref <- snpgdsPCA(genofile, snp.id = snpset.id, autosome.only = TRUE)

sample.id <- read.gdsn(index.gdsn(genofile, "sample.id"))
pop <- read.gdsn(index.gdsn(genofile, "sample.annot/pop.group"))

ref_pcs_base <- data.frame(
  sample.id = sample.id,
  pop = pop,
  PC1 = pca_ref$eigenvect[, 1],
  PC2 = pca_ref$eigenvect[, 2],
  stringsAsFactors = FALSE
)

# -----------------------
# 5) SNP metadata (restricted to pruned SNPs)
# -----------------------
ref_snp <- data.frame(
  snp.id = read.gdsn(index.gdsn(genofile, "snp.id")),
  rsid   = read.gdsn(index.gdsn(genofile, "snp.rs.id")),
  allele = read.gdsn(index.gdsn(genofile, "snp.allele")),
  stringsAsFactors = FALSE
)
ref_snp <- ref_snp[ref_snp$snp.id %in% snpset.id, ]

# -----------------------
# 6) Intersect by rsID
# -----------------------
common <- merge(ref_snp, jon, by.x = "rsid", by.y = "# rsid")
cat("Common SNPs (rsID intersection):", nrow(common), "\n")
stopifnot(nrow(common) > 100)

# -----------------------
# 7) Convert Jon genotype to dosage relative to GDS allele order "REF/ALT"
#    with strand-flip handling
# -----------------------
complement <- c(A="T", T="A", C="G", G="C")

get_dosage <- function(a1, a2, allele_string) {
  alleles <- strsplit(allele_string, "/", fixed = TRUE)[[1]]
  if (length(alleles) != 2) return(NA_real_)
  ref <- alleles[1]; alt <- alleles[2]
  
  g <- c(a1, a2)
  if (any(is.na(g))) return(NA_real_)
  if (!all(g %in% c("A","C","G","T"))) return(NA_real_)
  
  # direct match
  if (all(g %in% c(ref, alt))) {
    return(sum(g == alt))
  }
  
  # strand-flip match
  g_flip <- complement[g]
  if (all(g_flip %in% c(ref, alt))) {
    return(sum(g_flip == alt))
  }
  
  NA_real_
}

jon_dosage <- mapply(get_dosage, common$allele1, common$allele2, common$allele)
cat("NA dosage fraction:", mean(is.na(jon_dosage)), "\n")

# -----------------------
# 8) Pull reference genotypes for the intersected SNP list
#     ref_geno: (n_ref_samples x n_snps_common)
# -----------------------
ref_geno <- snpgdsGetGeno(genofile, snp.id = common$snp.id, with.id = FALSE)

# -----------------------
# 9) Initial keep filter: drop monomorphic SNPs & missing Jon calls
# -----------------------
p  <- colMeans(ref_geno, na.rm = TRUE) / 2
sd <- sqrt(2 * p * (1 - p))

keep <- is.finite(sd) & sd > 0 & !is.na(jon_dosage)
cat("SNPs kept after sd>0 & Jon present:", sum(keep), "of", length(keep), "\n")
stopifnot(sum(keep) > 200)

ref_geno2 <- ref_geno[, keep, drop = FALSE]
jon_use   <- jon_dosage[keep]

# -----------------------
# 10) Standardize safely (no scale(); avoid NA/Inf) + remove any remaining bad columns
# -----------------------
p2  <- colMeans(ref_geno2, na.rm = TRUE) / 2
sd2 <- sqrt(2 * p2 * (1 - p2))

keep2 <- is.finite(sd2) & sd2 > 0 & is.finite(p2) & !is.na(jon_use)
cat("Before keep2 SNPs:", ncol(ref_geno2), "\n")
cat("After  keep2 SNPs:", sum(keep2), "\n")
stopifnot(sum(keep2) > 200)

ref_geno3 <- ref_geno2[, keep2, drop = FALSE]
jon_use3  <- jon_use[keep2]
p3        <- p2[keep2]
sd3       <- sd2[keep2]

# Standardize reference and Jon using same parameters
ref_scaled <- sweep(ref_geno3, 2, 2 * p3, "-")
ref_scaled <- sweep(ref_scaled, 2, sd3, "/")

jon_scaled <- (jon_use3 - 2 * p3) / sd3

# Final paranoia: drop any SNP columns that still contain non-finite values
bad_cols <- !apply(ref_scaled, 2, function(v) all(is.finite(v)))
if (any(bad_cols)) {
  cat("Dropping non-finite columns after scaling:", sum(bad_cols), "\n")
  ref_scaled <- ref_scaled[, !bad_cols, drop = FALSE]
  jon_scaled <- jon_scaled[!bad_cols]
}

cat("Non-finite entries in ref_scaled:", sum(!is.finite(ref_scaled)), "\n")
stopifnot(sum(!is.finite(ref_scaled)) == 0)
stopifnot(all(is.finite(jon_scaled)))

# -----------------------
# 11) PCA via SVD on standardized reference matrix (version-independent)
#    ref_scaled = U D V^T
#    reference PC scores = U * D
#    new sample projection = jon_scaled %*% V
# -----------------------
svd_res <- svd(ref_scaled)

ref_PC1 <- svd_res$u[, 1] * svd_res$d[1]
ref_PC2 <- svd_res$u[, 2] * svd_res$d[2]

ref_pcs <- data.frame(
  PC1 = ref_PC1,
  PC2 = ref_PC2,
  pop = pop,
  stringsAsFactors = FALSE
)

V <- svd_res$v
stopifnot(length(jon_scaled) == nrow(V))

jon_pc1 <- sum(jon_scaled * V[, 1])
jon_pc2 <- sum(jon_scaled * V[, 2])

cat("Projected PCs:", jon_pc1, jon_pc2, "\n")

# -----------------------
# 12) Plot
# -----------------------
ggplot(ref_pcs, aes(PC1, PC2, color = pop)) +
  geom_point(alpha = 0.7) +
  geom_point(aes(x = jon_pc1, y = jon_pc2),
             inherit.aes = FALSE,
             color = "black",
             size = 4,
             shape = 17) +
  theme_bw()

# -----------------------
# 13) Close GDS (optional)
# -----------------------
# snpgdsClose(genofile)





#########
########
######
vcf_url <- "https://hgdownload.soe.ucsc.edu/gbdb/hg19/1000Genomes/phase3/ALL.chr10.phase3_shapeit2_mvncall_integrated_v5a.20130502.genotypes.vcf.gz"
tbi_url <- paste0(vcf_url, ".tbi")
old_method <- getOption("download.file.method")
options(download.file.method = "libcurl")
download.file(vcf_url, "chr10.vcf.gz", mode="wb")
download.file(tbi_url, "chr10.vcf.gz.tbi", mode="wb")
snpgdsVCF2GDS("chr10.vcf.gz", "chr10.gds", method="biallelic.only")
genofile <- snpgdsOpen("chr10.gds")
sample.id <- read.gdsn(index.gdsn(genofile, "sample.id"))

########## ONE BLOCK: chr10/chr22 1KGP GDS PCA + project Jon (robust + consistent SNP set) ##########

rm(list = ls())
showfile.gds(closeall = TRUE)
library(SNPRelate)
library(data.table)
library(plotly)

# ---- USER INPUTS ----
gds_path <- "chr10.gds"  # or "chr22.gds"
panel_path <- "panel.txt"
jon_path <- "/Users/jonathanchernus/Documents/Personal/medical/phased_genotype_data_Jon_Chernus/phased_genome_Jon_Chernus_v4_Full_20250403180654.txt"

# ---- OPEN DATA ----
showfile.gds(closeall = TRUE)
genofile <- snpgdsOpen(gds_path)

panel <- fread(panel_path, skip = 1)
setnames(panel, c("sample","pop","super_pop","gender"))

jon <- fread(jon_path)
if (!("# rsid" %in% names(jon)) && ("rsid" %in% names(jon))) setnames(jon, "rsid", "# rsid")
stopifnot("# rsid" %in% names(jon))
stopifnot(all(c("allele1","allele2") %in% names(jon)))

# ---- ALIGN SAMPLES (drop the 1 unmatched sample) ----
sample.id <- read.gdsn(index.gdsn(genofile, "sample.id"))
panel2 <- panel[match(sample.id, panel$sample), ]
keep_samp <- !is.na(panel2$sample)

cat("Samples in GDS:", length(sample.id), "\n")
cat("Matched to panel:", sum(keep_samp), "\n")

sample_use <- sample.id[keep_samp]
panel_use  <- panel2[keep_samp, ]

# ---- SNP META FROM GDS ----
snpid_gds <- read.gdsn(index.gdsn(genofile, "snp.id"))
rsid_gds  <- read.gdsn(index.gdsn(genofile, "snp.rs.id"))
alle_gds  <- read.gdsn(index.gdsn(genofile, "snp.allele"))

ref_snp_all <- data.frame(
  snp.id = snpid_gds,
  rsid   = rsid_gds,
  allele = alle_gds,
  stringsAsFactors = FALSE
)

# ---- Restrict to SNPs that exist in Jon by rsID (fast filter) ----
jon_rsid <- jon[["# rsid"]]
keep_in_jon <- rsid_gds %in% jon_rsid
snpid_in_jon <- snpid_gds[keep_in_jon]
cat("SNPs in GDS with rsID present in Jon:", length(snpid_in_jon), "\n")

# ---- LD PRUNE ON REFERENCE (only SNPs in Jon) ----
set.seed(1)
snpset <- snpgdsLDpruning(
  genofile,
  sample.id = sample_use,
  snp.id = snpid_in_jon,
  ld.threshold = 0.1,
  maf = 0.05,
  autosome.only = TRUE
)
snp_pruned <- unlist(snpset, use.names = FALSE)
cat("Pruned SNPs:", length(snp_pruned), "\n")
stopifnot(length(snp_pruned) > 200)

# ---- GET REFERENCE GENOTYPES (samples x SNPs) ----
ref_geno <- snpgdsGetGeno(
  genofile,
  sample.id = sample_use,
  snp.id = snp_pruned,
  with.id = FALSE
)

# ---- SNP annotation in EXACT SAME ORDER as ref_geno columns ----
ref_snp_pruned <- ref_snp_all[match(snp_pruned, ref_snp_all$snp.id), ]
stopifnot(all(ref_snp_pruned$snp.id == snp_pruned))

# ---- DROP STRAND-AMBIGUOUS SNPs (A/T and C/G) BEFORE PCA (critical consistency fix) ----
is_ambig <- function(a) {
  x <- strsplit(a, "/", fixed = TRUE)[[1]]
  if (length(x) != 2) return(TRUE)
  (x[1]=="A" && x[2]=="T") || (x[1]=="T" && x[2]=="A") ||
    (x[1]=="C" && x[2]=="G") || (x[1]=="G" && x[2]=="C")
}
ambig <- vapply(ref_snp_pruned$allele, is_ambig, logical(1))

# ---- STANDARDIZE REFERENCE (drop monomorphic + ambig) ----
p  <- colMeans(ref_geno, na.rm = TRUE) / 2
sd <- sqrt(2 * p * (1 - p))

keep_snp <- is.finite(p) & is.finite(sd) & sd > 0 & !ambig
cat("Kept SNPs for PCA/projection:", sum(keep_snp), "of", length(keep_snp), "\n")
stopifnot(sum(keep_snp) > 200)

ref_geno2   <- ref_geno[, keep_snp, drop = FALSE]
p2          <- p[keep_snp]
sd2         <- sd[keep_snp]
ref_snp_use <- ref_snp_pruned[keep_snp, ]   # <-- THIS defines the PCA SNP list/order

ref_scaled <- sweep(ref_geno2, 2, 2*p2, "-")
ref_scaled <- sweep(ref_scaled, 2, sd2, "/")
stopifnot(sum(!is.finite(ref_scaled)) == 0)

# ---- PCA via SVD (reference) ----
svd_res <- svd(ref_scaled)
V <- svd_res$v
stopifnot(nrow(V) == length(p2))

ref_pcs <- data.frame(
  sample = sample_use,
  pop = panel_use$pop,
  super_pop = panel_use$super_pop,
  PC1 = svd_res$u[,1] * svd_res$d[1],
  PC2 = svd_res$u[,2] * svd_res$d[2],
  PC3 = svd_res$u[,3] * svd_res$d[3],
  stringsAsFactors = FALSE
)

# ---- BUILD JON DOSAGES ON THE SAME SNP LIST (ref_snp_use$rsid) ----
# Strand-flip-aware dosage relative to "REF/ALT" string in ref_snp_use$allele
complement <- c(A="T", T="A", C="G", G="C")
get_dosage <- function(a1, a2, allele_string) {
  alleles <- strsplit(allele_string, "/", fixed = TRUE)[[1]]
  if (length(alleles) != 2) return(NA_real_)
  ref <- alleles[1]; alt <- alleles[2]
  g <- c(a1, a2)
  if (any(is.na(g))) return(NA_real_)
  if (!all(g %in% c("A","C","G","T"))) return(NA_real_)
  
  if (all(g %in% c(ref, alt))) return(sum(g == alt))
  
  g_flip <- complement[g]
  if (all(g_flip %in% c(ref, alt))) return(sum(g_flip == alt))
  
  NA_real_
}

# Join Jon genotypes to the SNP list we used for PCA (by rsid)
jon_sub <- jon[match(ref_snp_use$rsid, jon[["# rsid"]]), ]
# jon_sub rows correspond to ref_snp_use order, but may have NAs if rsid not found
# Make dosage vector in PCA SNP order:
jon_vec <- rep(NA_real_, nrow(ref_snp_use))
# If Jon is far from EUR, the allele-count convention is inverted for this GDS.
# Empirically, we need to flip dosage:


found <- !is.na(jon_sub[["# rsid"]])
jon_vec[found] <- mapply(
  get_dosage,
  jon_sub$allele1[found],
  jon_sub$allele2[found],
  ref_snp_use$allele[found]
)
jon_vec <- 2 - jon_vec
cat("Jon missing fraction on PCA SNP set:", mean(is.na(jon_vec)), "\n")

# Standardize with reference p2/sd2; mean-impute missing to 0 in standardized space
jon_scaled <- (jon_vec - 2*p2) / sd2
jon_scaled[!is.finite(jon_scaled)] <- 0

# Project Jon into same PC space
jon_pc1 <- sum(jon_scaled * V[,1])
jon_pc2 <- sum(jon_scaled * V[,2])
jon_pc3 <- sum(jon_scaled * V[,3])

cat("Jon PCs:", jon_pc1, jon_pc2, jon_pc3, "\n")

# Optional: quick "are we insane?" range check
cat("Ref PC1 range:", paste(range(ref_pcs$PC1), collapse=" .. "), "\n")
cat("Ref PC2 range:", paste(range(ref_pcs$PC2), collapse=" .. "), "\n")
cat("Ref PC3 range:", paste(range(ref_pcs$PC3), collapse=" .. "), "\n")

# Optional: orientation test (if Jon still way off, see if flipping fixes it)
jon_scaled_flip <- ((2 - jon_vec) - 2*p2) / sd2
jon_scaled_flip[!is.finite(jon_scaled_flip)] <- 0
jon_pc1_flip <- sum(jon_scaled_flip * V[,1])
jon_pc2_flip <- sum(jon_scaled_flip * V[,2])
jon_pc3_flip <- sum(jon_scaled_flip * V[,3])
cat("Jon PCs if dosage flipped:", jon_pc1_flip, jon_pc2_flip, jon_pc3_flip, "\n")

# ---- 3D PLOT ----
p <- plot_ly(
  ref_pcs,
  x = ~PC1, y = ~PC2, z = ~PC3,
  type = "scatter3d",
  mode = "markers",
  color = ~pop,
  text = ~paste0(sample, "<br>", pop, " (", pop, ")"),
  hoverinfo = "text",
  marker = list(size = 3)
)

p <- add_trace(
  p,
  x = c(jon_pc1), y = c(jon_pc2), z = c(jon_pc3),
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 7, symbol = "diamond", color = "black"),
  text = "Jon",
  hoverinfo = "text",
  name = "Jon",
  inherit = FALSE
)

p


# ===================== KNN (pop + super_pop) on PCA coords + add Jon + plot with predicted color =====================
# Assumes you already have:
#   ref_pcs (data.frame with columns: sample, pop, super_pop, PC1, PC2, PC3)
#   jon_pc1, jon_pc2, jon_pc3 (numeric)
#   p (your existing plotly object created from ref_pcs; or you can recreate it below)

k_list <- c(3, 5, 7, 11, 21, 51)

# ---- helper: majority vote with tie-break by nearest-neighbor (distance) ----
knn_vote <- function(labels, dists) {
  tab <- sort(table(labels), decreasing = TRUE)
  top_n <- as.integer(tab[1])
  tied <- names(tab)[tab == top_n]
  if (length(tied) == 1) return(tied)
  # tie-break: among tied classes, pick the class of the single closest neighbor
  labels[which.min(dists)]
}

# ---- compute distances in 3D PC space ----
ref_mat <- as.matrix(ref_pcs[, c("PC1","PC2","PC3")])
jon_vec <- c(jon_pc1, jon_pc2, jon_pc3)
d <- sqrt(rowSums((ref_mat - matrix(jon_vec, nrow(ref_mat), 3, byrow = TRUE))^2))

ord <- order(d)

# ---- run KNN for multiple k and report ----
knn_results <- rbindlist(lapply(k_list, function(k) {
  idx <- ord[seq_len(k)]
  sp <- knn_vote(ref_pcs$super_pop[idx], d[idx])
  pp <- knn_vote(ref_pcs$pop[idx],       d[idx])
  data.table(k = k, pred_super_pop = sp, pred_pop = pp)
}))

print(knn_results)

# ---- pick a final k (majority among k's, tie-break by smallest k) ----
final_super <- names(sort(table(knn_results$pred_super_pop), decreasing = TRUE))[1]
final_pop   <- names(sort(table(knn_results$pred_pop),       decreasing = TRUE))[1]

cat("FINAL predicted super_pop:", final_super, "\n")
cat("FINAL predicted pop:      ", final_pop, "\n")

# ---- add Jon to ref_pcs with predicted labels (so he shares the SAME color mapping) ----
jon_row <- data.frame(
  sample = "Jon",
  pop = final_pop,
  super_pop = final_super,
  PC1 = jon_pc1, PC2 = jon_pc2, PC3 = jon_pc3,
  stringsAsFactors = FALSE
)

ref_plus_jon <- rbind(ref_pcs, jon_row)

# ---- plot: keep reference colored by super_pop; add Jon as same color (mapped) but unique symbol ----
p2 <- plot_ly(
  ref_plus_jon,
  x = ~PC1, y = ~PC2, z = ~PC3,
  type = "scatter3d",
  mode = "markers",
  color = ~pop,
  text = ~paste0(sample, "<br>", pop, " (", pop, ")"),
  hoverinfo = "text",
  marker = list(size = 3),
  showlegend = TRUE
)

# add Jon as separate trace so we can force the unique symbol/size, but keep color consistent:
jon_color <- unique(ref_plus_jon$super_pop[ref_plus_jon$sample == "Jon"])
# plotly won't let us directly "inherit" the exact palette color reliably without reusing the same mapping;
# easiest is: add Jon as a filtered trace that still maps color by super_pop, then override symbol/size
p2 <- add_trace(
  p2,
  data = subset(ref_plus_jon, sample == "Jon"),
  x = ~PC1, y = ~PC2, z = ~PC3,
  type = "scatter3d",
  mode = "markers",
  color = ~pop,                 # <-- keeps same color mapping
  marker = list(size = 8, symbol = "diamond", line = list(width = 1)),
  text = ~paste0("Jon<br>", pop, " (", pop, ")"),
  hoverinfo = "text",
  name = "Jon",
  inherit = FALSE,
  showlegend = FALSE
)

p2

library(plotly)

p3 <-
  ggplot() +
  # Reference samples
  geom_point(
    data = subset(ref_plus_jon, sample != "Jon"),
    aes(
      x = PC1,
      y = PC2,
      color = pop,
      text = paste0(
        "Sample: ", sample,
        "<br>Pop: ", pop,
        "<br>SuperPop: ", super_pop
      )
    ),
    size = 2,
    alpha = 0.7
  ) +
  # Jon on top
  geom_point(
    data = subset(ref_plus_jon, sample == "Jon"),
    aes(
      x = PC1,
      y = PC2,
      text = paste0(
        "Sample: Jon",
        "<br>Predicted Pop: ", pop,
        "<br>Predicted SuperPop: ", super_pop
      )
    ),
    size = 4,
    shape = 18,
    color = "black"
  )

# Convert to interactive
ggplotly(p3, tooltip = "text")


# ===================== end KNN block =====================
