library(readxl)
meta <- read_xlsx("input/metadata_caudaEVs.xlsx")

ex_files <- list.files(path = "input", pattern = "exceRpt", full.names = T)
excerpt <- ldply(lapply(ex_files, function(x) {
  a <- read.table(x, header = T, stringsAsFactors = F)
  a$Genes <- rownames(a)
  return(a)
  }))

rr_files <- list.files("input/rRNA", pattern = ".txt", full.names = T)
names(rr_files) <- gsub(pattern = "input/rRNA/|_rrnaCounts.txt", replacement = "", x = rr_files)
rRNA <- lapply(rr_files, function(x) {
  a <- read.table(x, header = F, stringsAsFactors = F)
  return(a)
})

for(i in 1:length(rRNA)){
  colnames(rRNA[[i]]) <- c("Genes", names(rRNA)[i])
}

rRNA_mat <- Reduce(merge, rRNA)

a <- filterByExpr(y = DGEList(rRNA_mat[,-1]))

View(rRNA_mat[a,])

saveRDS(rRNA_mat, "rRNA.rds")
