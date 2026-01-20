---
  title: "UCSC Genome Browser query examples"
output:
  html_document:
  toc: true
toc_float: true
code_folding: show
---

```{r setup, include=FALSE}
#knitr::opts_chunk$set(echo = TRUE)
library(RMySQL)
#defaultWarnings <- getOption("warn") 
#options(warn = -1) # MySQL can give you some odd warnings,
#                   # so we are temporarily turning off warnings
query <- function(...) dbGetQuery(mychannel, ...)
#textstyle <- function(x) {
#  format(x, big.mark = ",", scientific = FALSE)
#}
```