
if ((Sys.Date() - as.Date(file.info("data/COSMIC_sig.rdata", extra_cols = TRUE)$mtime) )>30){
  download.file("https://cancer.sanger.ac.uk/cancergenome/assets/signatures_probabilities.txt",
                "data/sig.txt", mode = 'wb')
  sig_file <- read.delim("data/sig.txt") 
  sig_full <- sig_file[order(sig_file[,1]),1:33]
  
  save(sig_full, file="data/COSMIC_sig.rdata")
  
}


if ((Sys.Date() - as.Date(file.info("data/COSMIC.rdata", extra_cols = TRUE)$mtime) )>30){
  
  cosmic <- read_html("https://cancer.sanger.ac.uk/cosmic/signatures")
  
  comment <- html_nodes(cosmic, "div") %>% html_nodes("span") %>% html_text() %>% gsub("[\r\n]", "", .)
  
  comment_full_mat <- matrix(comment[-1], 30, 4, byrow = TRUE)
  
  colnames(comment_full_mat) <- c("Cancer types", "Proposed aetiology",
                                  "Additional mutational features", "Comments")
  save(comment_full_mat, file="data/COSMIC_comment.rdata")
}

load("data/Signaturelog.RData")
load("data/COSMIC.rdata")
load("data/COSMIC_sig.rdata")

pm_corr <- read.csv("data/pm_corr.csv", na = "0") %>% as.matrix()
pm_corr[is.na(pm_corr)] <- 0
rownames(pm_corr) <- paste0("P", 1:27)

cosmic_corr <- read.csv("data/cosmic_corr.csv", na = "0") %>% as.matrix()
cosmic_corr[is.na(cosmic_corr)] <- 0
rownames(cosmic_corr) <- c(paste0("C", 1:30), "Other")


library(pmsignature)
library(rvest)
library(purrr)
library(magrittr)
library(ggplot2)
library(corrplot)


visPMS_full_modified(sig_full[,4], 3, FALSE)


corr_mat <- matrix(NA, length(Fs), 30)
for(i in 1:length(Fs)){
  full_sig <- convertSignatureMatrixToVector(Fs[[i]], c(6,4,4))
  
  corr_mat[i,] <- sapply(1:30, function(x) getCosDistance(full_sig,sig_full[,x+3]))
}

rownames(corr_mat) <- paste0("P", 1:length(Fs))
colnames(corr_mat) <- paste0("C", 1:30)


myCol <- colorRampPalette(c("#F8F8FF", "#F8F8FF", "#F8F8FF", "#6B8E23"))

url_share <- "https://twitter.com/intent/tweet?text=Excited%20to%20share%20this%20Shiny%20app%20with%20you&url=http://www.github.com/USCbiostat/iMutSig"
url_cite <- "http://www.github.com/USCbiostats/iMutSig"
