# Read data
gtex <- read_tsv(
  "/Users/jonathanchernus/Documents/Teaching/hugen2073-textbook/exercises/7_categorical_data_and_proportions/GTEx_Analysis_2017-06-05_v8_RNASeQCv1.1.9_gene_median_tpm.gct.gz",
  skip = 2
)

# Inspect
glimpse(gtex)
gtex_long <- gtex %>%
  rename(
    gene_id = Name,
    gene_symbol = Description
  ) %>%
  pivot_longer(
    cols = -c(gene_id, gene_symbol),
    names_to = "tissue",
    values_to = "median_tpm"
  )
gtex_long <- gtex_long %>%
  filter(median_tpm > 0.1)

# install.packages("msigdbr")

msig_hallmark <- msigdbr(
  species = "Homo sapiens",
  category = "H"
)
write.csv(msig_hallmark,
          file="/Users/jonathanchernus/Documents/Teaching/hugen2073-textbook/exercises/7_categorical_data_and_proportions/msig_hallmark.csv",
          quote=FALSE,
          row.names=FALSE)

gtex_hallmark <- gtex_long %>%
  left_join(
    msig_hallmark_pairs,
    by = "gene_symbol",
    relationship = "many-to-many"
  ) %>%
  mutate(
    gs_name = if_else(is.na(gs_name), "Not_in_Hallmark", gs_name),
    log_tpm = log10(median_tpm + 1),
    expressed_high = median_tpm > 10
  )

write.csv(gtex_hallmark,
          file="/Users/jonathanchernus/Documents/Teaching/hugen2073-textbook/exercises/7_categorical_data_and_proportions/gtex_hallmark.csv",
          quote=FALSE,
          row.names=FALSE)