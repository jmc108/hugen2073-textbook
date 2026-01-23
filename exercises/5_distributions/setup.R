# chr22 VCF
#curl -O https://ftp.1000genomes.ebi.ac.uk/vol1/ftp/release/20130502/ALL.chr22.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz
#curl -O https://ftp.1000genomes.ebi.ac.uk/vol1/ftp/release/20130502/ALL.chr22.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz.tbi

# population panel
#curl -O https://ftp.1000genomes.ebi.ac.uk/vol1/ftp/release/20130502/integrated_call_samples_v3.20130502.ALL.panel

#
# ./plink_mac_20250819/plink --vcf ALL.chr22.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz --het --out chr22

het <- read.table("~/Downloads/chr22.het", header=TRUE)

panel <- read_tsv("~/Downloads/integrated_call_samples_v3.20130502.ALL.panel",show_col_types = FALSE) |>
  rename(IID = sample)

d <- het |>
  left_join(panel, by = "IID") |>
  # pick 10–20 populations with the most samples
  add_count(pop, name = "n_pop") |>
  filter(n_pop >= 80) |>
  mutate(pop = fct_reorder(pop, F))

write.csv(d, file="/Users/jonathanchernus/Documents/Teaching/hugen2073-textbook/exercises/5_distributions/het.csv", row.names = FALSE, quote = FALSE)