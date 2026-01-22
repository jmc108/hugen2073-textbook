library(tidyverse)
library(vroom)

clinvar_url <- "https://ftp.ncbi.nlm.nih.gov/pub/clinvar/tab_delimited/variant_summary.txt.gz"

cv <- vroom::vroom(
  clinvar_url,
  delim = "\t",
  col_select = c(GeneSymbol, Type, ClinicalSignificance),
  show_col_types = FALSE
)

# Keep a clean 5-level ordinal ClinicalSignificance
clinsig_levels <- c("Benign", "Likely benign", "Uncertain significance",
                    "Likely pathogenic", "Pathogenic")

cv2 <- cv |>
  filter(GeneSymbol %in% c("BRCA1", "BRCA2")) |>
  mutate(
    # ClinicalSignificance can be comma-separated; take the first for a simple teaching dataset
    clinsig = str_split_i(ClinicalSignificance, ",", 1) |> str_trim(),
    clinsig = factor(clinsig, levels = clinsig_levels, ordered = TRUE),
    Type = fct_lump_n(Type, n = 6)  # lump rare types into "Other"
  ) |>
  filter(!is.na(clinsig))

write.csv(x=cv2, file="/Users/jonathanchernus/Documents/Teaching/hugen2073-textbook/exercises/4_amounts_and_tables/cv.csv", quote = FALSE)