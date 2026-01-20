library("data.table") # For the fread function
library("qqman") # For Manhattan and Q-Q plots
library("GWASTools") # Another package for Manhattan and Q-Q plots
library("tidyverse")

# Download the data from http://ftp.ebi.ac.uk/pub/databases/gwas/summary_statistics/GCST008001-GCST009000/GCST008672/
# (Or try using a different set of summary stats from the GWAS catalog)
# No need to decompress it, because fread can do that
# You *could* decompress the file and then reading in kneepain2_f6159_v3_1812.bgenie.txt with the base R read.table function
# But that would take a lot longer!
# The "gunzip -c" part applies pre-processes the file by decompressing it in place (i.e., the original file is not altered or copied)
d <- fread("gunzip -c ~/Downloads/kneepain2_f6159_v3_1812.bgenie.txt.gz") 

# Make a manhattan plot using qqman::manhattan
# Notice it will be SLOW, and we can see this by timing it
# (I mean really slow. Like 10 minutes or more. Don't wait for it to finish! Just cancel the command and restart R if you have to.)
t0 <- proc.time() # This starts a "stopwatch"
manhattan(x=d, chr="chr", bp="pos", p="p", snp="rsid")
(proc.time() - t0)[3] # This number is how long it took, in seconds

# Instead, we can speed it up by filtering out a lot of SNPs
# Now if we want to tinker with some of the plot settings, we can re-draw it very quickly
# (intead of having to wait 10 minutes for the plot to be re-drawn every time we make a tiny change)
d_filtered <- d %>% filter(p < 0.01) # This means we only keep SNPS with -log10(p) > 2
nrow(d_filtered)/nrow(d) # We only kept about 1.2% of the SNPs
t0 <- proc.time() # This restarts the timer
manhattan(x=d_filtered, chr="chr", bp="pos", p="p", snp="rsid")
(proc.time() - t0)[3] # Time, in seconds (on my computer it took about 2.2 seconds)

# Suppose I want to highlight and annotate the genome-wide significant SNPS
manhattan(x=d_filtered, chr="chr", bp="pos", p="p", snp="rsid",
          highlight=d_filtered$rsid[d_filtered$p < 5e-8],
          annotatePval = 5e-8)

# How to make a Q-Q plot?
# Remember to use the full dataset, not the filtered version
# This will be slow (at least several minutes)
# It will probably be faster to save the plot as a png than to view it in RStudio!
qq(d$p)


# How to calculate the inflation factor, lambda?
chisq <- qchisq(1-d$p,1) # Convert p-values to corresponding chi-square statistics
lambda <- median(chisq)/qchisq(0.5,1) # How much bigger is your median chi-square statistic than that of a 1-df chi-square distribution's?
lambda

# Now let's try the GWASTools package
# The manhattanPlot function has different parameters than manhattan function we used above
# We don't need to filter the p-values
# Instead, it can *thin* the p-values for us (for the least significant SNPs, it plots only a fraction of them)
# Also notice that this function doesn't use the base-pair position at all (read the documentation!)
# Try experimenting with thinThreshold and ylim
manhattanPlot(p=d$p, chromosome=d$chr,
              thinThreshold=2,
              ylim=c(0,10))

# And a q-q plot
# Here it applies the thinning threshold (but the quantiles are still correctly calculated)
qqPlot(p=d$p, thinThreshold=2)

# For nicer and more customizable and interactive plots, check out https://r-graph-gallery.com/101_Manhattan_plot.html