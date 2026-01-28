library("GWASTools")
library("GWASdata")
data(illumina_snp_annot)
# Create a SnpAnnotationDataFrame
snpAnnot <- SnpAnnotationDataFrame(illumina_snp_annot)
data(illumina_scan_annot)
# Create a ScanAnnotationDataFrame
scanAnnot <- ScanAnnotationDataFrame(illumina_scan_annot)

blfile <- system.file("extdata", "illumina_bl.gds", package="GWASdata")
blgds <- GdsIntensityReader(blfile)
blData <- IntensityData(blgds, snpAnnot=snpAnnot, scanAnnot=scanAnnot)
genofile <- system.file("extdata", "illumina_geno.gds", package="GWASdata")
genogds <- GdsGenotypeReader(genofile)
genoData <- GenotypeData(genogds, snpAnnot=snpAnnot, scanAnnot=scanAnnot)


baf.sd <- sdByScanChromWindow(blData, genoData, var="BAlleleFreq")
med.baf.sd <- medianSdOverAutosomes(baf.sd)
low.qual.ids <- med.baf.sd$scanID[med.baf.sd$med.sd > 0.05]

chrom <- getChromosome(snpAnnot, char=TRUE)
pos <- getPosition(snpAnnot)
hla.df <- get(data(HLA.hg18))
hla <- chrom == "6" & pos >= hla.df$start.base & pos <= hla.df$end.base
xtr.df <- get(data(pseudoautosomal.hg18))
xtr <- chrom == "X" & pos >= xtr.df["X.XTR", "start.base"] &
   pos <= xtr.df["X.XTR", "end.base"]
centromeres <- get(data(centromeres.hg18))
gap <- rep(FALSE, nrow(snpAnnot))
for (i in 1:nrow(centromeres)) {
   ingap <- chrom == centromeres$chrom[i] & pos > centromeres$left.base[i] &
     pos < centromeres$right.base[i]
   gap <- gap | ingap
   }
ignore <- snpAnnot$missing.n1 == 1 #ignore includes intensity-only and failed snps
snp.exclude <- ignore | hla | xtr | gap
#snp.ok <- snpAnnot$snpID[!snp.exclude]
snp.ok <- snpAnnot$snpID[]

scan.ids <- scanAnnot$scanID[1:10]
chrom.ids <- 21:23
baf.seg <- anomSegmentBAF(blData, genoData, scan.ids=scan.ids,
                             chrom.ids=chrom.ids, snp.ids=snp.ok, verbose=FALSE)
head(baf.seg)

baf.anom <- anomFilterBAF(blData, genoData, segments=baf.seg,
                             snp.ids=snp.ok, centromere=centromeres, low.qual.ids=low.qual.ids,
                             verbose=FALSE)
names(baf.anom)

baf.filt <- baf.anom$filtered
head(baf.filt)


loh.anom <- anomDetectLOH(blData, genoData, scan.ids=scan.ids,
                             chrom.ids=chrom.ids, snp.ids=snp.ok, known.anoms=baf.filt,
                             verbose=FALSE)
names(loh.anom)

loh.filt <- loh.anom$filtered
head(loh.filt)





# create required data frame
baf.filt$method <- "BAF"
if (!is.null(loh.filt)) {
   loh.filt$method <- "LOH"
   cols <- intersect(names(baf.filt), names(loh.filt))
   anoms <- rbind(baf.filt[,cols], loh.filt[,cols])
   } else {
     anoms <- baf.filt
     }
anoms$anom.id <- 1:nrow(anoms)
stats <- anomSegStats(blData, genoData, snp.ids=snp.ok, anom=anoms,
                         centromere=centromeres)
names(stats)
snp.not.ok <- character(0)

anomStatsPlot(blData, genoData, anom.stats=stats[1,],
              snp.ineligible=snp.not.ok, centromere=centromeres, cex.leg=1)


# Here's a chromosomal anomaly
indices <- which(pData(snpAnnot)$chromosome==22)
lrr <- getLogRRatio(blData)[xx,7]
positions <- pData(snpAnnot)$position[indices]
plot(positions,lrr)
d <- data.frame(pos=positions, lrr=lrr)
write.csv(d, file="/Users/jonathanchernus/Documents/Teaching/hugen2073-textbook/exercises/6_associations_and_trends/lrr.csv",
          quote=FALSE, row.names=FALSE)

##############


#install.packages("BiocManager")
#BiocManager::install("BSgenome.Hsapiens.UCSC.hg38")

library(BSgenome.Hsapiens.UCSC.hg38)

genome <- BSgenome.Hsapiens.UCSC.hg38

seq_region <- genome$chr19[44901944:44905541]
bases <- strsplit(as.character(seq_region), "")[[1]]

gc_binary <- ifelse(bases %in% c("G","C"), 1, 0)


#install.packages("zoo")
library(zoo)
library("tidyverse")





ggplot(df_long, aes(position, gc)) +
  #geom_area(fill = "black", alpha = 0.4)+
  #geom_line(linewidth = 0.1, color="black") +
  #coord_cartesian(xlim = c(1, 20000)) +
  labs(x = "Genomic position (bp)", y = "GC fraction") +
  theme_minimal() +
  ggplot(df, aes(position)) +
  geom_ribbon(aes(ymin = 0, ymax = gc), fill = "black", alpha = 0.4)
  #geom_line(aes(y = gc), color = "black", linewidth = 0.7)


bin_size <- 5  # set to whatever you used






k <- 5
df <- data.frame(
  position = seq_along(gc_binary),
  GC_100  = zoo::rollmean(gc_binary, k = k,  fill = NA, align = "center")
  #GC_500  = zoo::rollmean(gc_binary, k = 10,  fill = NA, align = "center"),
  #GC_1000 = zoo::rollmean(gc_binary, k = 20, fill = NA, align = "center")
)


df_long <- df |>
  pivot_longer(-position, names_to = "window", values_to = "gc") |>
  dplyr::filter(!is.na(gc))
ggplot(df_long,
       aes(x = position, y = gc)) +
  geom_line(width = 1) +
  ylim(c(0,1)) +
  xlim(c(0,500))