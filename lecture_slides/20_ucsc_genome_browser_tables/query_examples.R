# Load package (install it first if you need to)
library(RMySQL)

# Create a connection to the database - this gets used in each query
mychannel <- dbConnect(MySQL(),
                       user="genome",
                       host="genome-mysql.cse.ucsc.edu")


# Example 1
# Notice the query is in double quotes and uses single quotes inside where needed
# Notice it can be spaced over multiple lines (whitespace doesn't matter)
# The query ends with a semicolon
data1 <- dbGetQuery(mychannel,
                   "SELECT chromEnd FROM hg38.snp151
                   WHERE name = 'rs3';")
dim(data1)
head(data1)
tail(data1)

# Example 2 - find genes near a GWAS peak
# Suppose you have a GWAS hit at 4490822 on chr19
# Using paste0 avoids hard-coding
chr <- "chr19"
pos <- 44908822
upstream <- pos - 10000
downstream <- pos + 10000
sql <- paste0("SELECT name2 ",
              "FROM hg38.wgEncodeGencodeBasicV39 ",
              "WHERE (chrom = '", chr, "' AND ",
              "strand = '+' AND ",
              "txEnd > ", upstream, " AND ",
              "txStart < ", downstream, ") OR ",
              "(chrom = '", chr, "' AND ",
              "strand = '-' AND ",
              "txStart > ", upstream, " AND ",
              "txEnd < ", pos + 10000, ");")
data2 <- dbGetQuery(mychannel, sql) %>% distinct()
data2

# Example 3
data3 <- dbGetQuery(mychannel,
                   "SELECT chrom, chromStart, chromEnd, name, refNCBI
                   FROM hg38.snp151
                   WHERE chrom = 'chr17'
                   AND chromStart >= 7571720
                   AND chromEnd <= 7590868
                   ORDER BY chrom, chromStart;")
dim(data3)
head(data3)
tail(data3)

# Example 4
# Note this is slightly different from the example on slide 34
# Slide 34 refers to a 'func' variable that does not exist in the GWAS Catalog
data4 <- dbGetQuery(mychannel,
                    "SELECT hg38.snp150.name, hg38.snp150.chrom, hg38.snp150.chromEnd, hg38.gwasCatalog.genes
                    
                    FROM hg38.snp150 JOIN hg38.gwasCatalog
                      ON hg38.snp150.name = hg38.gwasCatalog.name
                    
                    WHERE hg38.gwasCatalog.trait = 'Pancreatic cancer' AND
                    hg38.gwasCatalog.pValue < 5e-8
                    
                    ORDER BY hg38.snp150.chrom, hg38.snp150.chromEnd;")
dim(data4)
head(data4)
tail(data4)



# Disconnect
dbDisconnect(mychannel)

