# Example
# Suppose my differential expression analysis gave me these genes:
#   SYCP1, SYCP2, SYCP3, SYCE1, SYCE2, SYCE3, BRCA1, TP53, AKT1, EGFR, MYC, APOE, CYP2D6, VEGFA, MTHFR, FTO
# I notice there are a lot of synaptonemal complex genes and want to test for over-representation


# Suppose I want to focus on the "synaptonemal complex assembly" GO term gene set
# These are the numbers I need to calculate a p-value
n_my_genes <- 16 # How many genes are on my list
N_genome <- 20580 # How many genes are in the genome
n_gene_set <- 24 # How many genes are in the "synaptonemal complex assembly" GO term gene set
n_overlap <- 5 # How many genes on my list are in that gene set

# Probability of picking 16 genes out of the genome,
#   AND exactly 5 of them were in that gene set
(choose(n_gene_set,n_overlap)*choose(N_genome-n_gene_set,n_my_genes-n_overlap))/choose(N_genome,n_my_genes)

# Actually, the p-value is the probability of getting an overlap of AT LEAST that size (5 or 6 or 7 or ....)
p_value <- 0
max_k <- min(n_my_genes, n_gene_set)
for (k in n_overlap:max_k) {
  p_value <- p_value + (choose(n_gene_set,k)*choose(N_genome-n_gene_set,n_my_genes-k))/choose(N_genome,n_my_genes)
}
p_value