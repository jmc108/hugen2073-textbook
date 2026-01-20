# This code is adapted slightly from https://uw-gac.github.io/topmed_workshop_2017/pc-relate.html
# I also had to manually download https://github.com/UW-GAC/analysis_pipeline,
# And then run the command "Rscript install_packages.R" in the Unix terminal
# from within the analysis_pipeline-master directory
# Or do install_github("UW-GAC/analysis_pipeline/TopmedPipeline/")

# Load a package we need
library("GWASTools")

# use a GDS file with all chromosomes
library("SeqArray")
data.path <- "https://github.com/UW-GAC/analysis_pipeline/raw/master/testdata"
gdsfile <- "1KG_phase3_subset.gds"
if (!file.exists(gdsfile)) download.file(file.path(data.path, gdsfile), gdsfile)
gds <- seqOpen(gdsfile)

# use a subset of 100 samples to make things run faster
workshop.path <- "https://github.com/UW-GAC/topmed_workshop_2017/raw/master"
sampfile <- "samples_subset100.RData"
if (!file.exists(sampfile)) download.file(file.path(workshop.path, sampfile), sampfile)
sample.id <- TopmedPipeline::getobj(sampfile)

# LD pruning to get variant set
library(SNPRelate)
snpset <- snpgdsLDpruning(gds, sample.id=sample.id, method="corr", 
                          slide.max.bp=10e6, ld.threshold=sqrt(0.1))
sapply(snpset, length)
pruned <- unlist(snpset, use.names=FALSE)

# KING
king <- snpgdsIBDKING(gds, sample.id=sample.id, snp.id=pruned)
names(king)
dim(king$kinship)
kingMat <- king$kinship
colnames(kingMat) <- rownames(kingMat) <- king$sample.id
kinship <- snpgdsIBDSelection(king)
head(kinship)

library(ggplot2)
ggplot(kinship, aes(IBS0, kinship)) +
  geom_hline(yintercept=2^(-seq(3,9,2)/2), linetype="dashed", color="grey") +
  geom_point(alpha=0.5) +
  ylab("kinship estimate") +
  theme_bw()

#PC-AiR
library(GENESIS)
sampset <- pcairPartition(kinobj=kingMat,
                          kin.thresh=2^(-9/2),
                          divobj=kingMat,
                          div.thresh=-2^(-9/2))
names(sampset)
sapply(sampset, length)

# run PCA on unrelated set
pca.unrel <- snpgdsPCA(gds, sample.id=sampset$unrels, snp.id=pruned)

# project values for relatives
# First calculate the SNP loadings
# Ten calculate the sample eigenvectors using the specified SNP loadings
snp.load <- snpgdsPCASNPLoading(pca.unrel, gdsobj=gds)
samp.load <- snpgdsPCASampLoading(snp.load, gdsobj=gds, sample.id=sampset$rels)

# combine unrelated and related PCs and order as in GDS file
pcs <- rbind(pca.unrel$eigenvect, samp.load$eigenvect)
rownames(pcs) <- c(pca.unrel$sample.id, samp.load$sample.id)
samp.ord <- match(sample.id, rownames(pcs))
pcs <- pcs[samp.ord,]

# Which PCs are ancestry-informative?
# Need ancestry info, so load it
sampfile <- "sample_annotation.RData"
if (!file.exists(sampfile)) download.file(file.path(workshop.path, sampfile), sampfile)
annot <- TopmedPipeline::getobj(sampfile)
annot
head(pData(annot))
varMetadata(annot)


# Make a parallel coordinates plot of the PCs
# Color by ancestry
pc.df <- as.data.frame(pcs)
names(pc.df) <- 1:ncol(pcs)
pc.df$sample.id <- row.names(pcs)

library(dplyr)
annot2 <- pData(annot) %>%
  select(sample.id, Population)
pc.df <- left_join(pc.df, annot2, by="sample.id")

library(GGally)
library(RColorBrewer)
pop.cols <- setNames(brewer.pal(12, "Paired"),
                     c("ACB", "ASW", "CEU", "GBR", "CHB", "JPT", "CLM", "MXL", "LWK", "YRI", "GIH", "PUR"))
ggparcoord(pc.df, columns=1:12, groupColumn="Population", scale="uniminmax") +
  scale_color_manual(values=pop.cols) +
  xlab("PC") + ylab("")

# Close the gds file from above
showfile.gds(closeall=TRUE, verbose=TRUE)
rm(list=ls())

