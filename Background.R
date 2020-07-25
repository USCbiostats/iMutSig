library(pmsignature)
library(rvest)
library(corrplot)
source('functions/extraFunctions.R')

########################################################################################
## This part needs to be manually updated whenever there is a new update on signatures. 
########################################################################################

Update <- FALSE
if (Update){
  download.file("https://cancer.sanger.ac.uk/cancergenome/assets/signatures_probabilities.txt",
                "data/sig_v2.txt", mode = 'wb')
  sig_file_v2 <- read.delim("data/sig_v2.txt") 
  sig_full_v2 <- sig_file_v2[order(sig_file_v2[,1]),1:33]
  
  download.file("https://cancer.sanger.ac.uk/signatures/COSMIC_Mutational_Signatures_v3.1.xlsx",
                "data/sig_v3.1.xlsx", mode = 'wb')
  sig_file_v3 <- readxl::read_excel("data/sig_v3.1.xlsx") 
  sig_full_v3 <- sig_file_v3[order(sig_file_v3[,1]),]
  sig_full_v3 <- data.frame(sig_full_v3)
  
  save(sig_full_v2, sig_full_v3, file="data/COSMIC_sig.rdata")
}

load("data/Signaturelog.RData")
load("data/COSMIC_sig.rdata")

pm_corr <- read.csv("data/pm_corr.csv", na = "0") %>% as.matrix()
pm_corr[is.na(pm_corr)] <- 0
rownames(pm_corr) <- paste0("P", 1:27)

# page 1, v2
cosmic_corr_v2 <- read.csv("data/cosmic_corr.csv", na = "0") %>% as.matrix()
cosmic_corr_v2[is.na(cosmic_corr_v2)] <- 0
rownames(cosmic_corr_v2) <- c(paste0("C", 1:30), "Other")

# page 1, v3
cosmic_corr_v3_raw <- read.csv("data/PCAWG_sigProfiler_SBS_signatures_in_samples.csv", header = TRUE)
cosmic_corr_v3 <- sapply(unique(cosmic_corr_v3_raw$Cancer.Types), function(x){
  cosmic_corr_v3_raw %>% filter(Cancer.Types==x) %>% select(-c(1:3)) %>% sapply(., function(y) mean(y!=0))
})
colnames(cosmic_corr_v3) <- unique(cosmic_corr_v3_raw$Cancer.Types)
missingSig <- setdiff(colnames(sig_full_v3)[-c(1:2)], rownames(cosmic_corr_v3))
for(i in seq_along(missingSig)){
  cosmic_corr_v3 <- rbind(cosmic_corr_v3, 0)
  rownames(cosmic_corr_v3)[nrow(cosmic_corr_v3)] <- missingSig[i]
}

# cosmic v2
corr_mat_v2 <- matrix(NA, length(Fs), 30)
for(i in 1:length(Fs)){
  full_sig <- convertSignatureMatrixToVector(Fs[[i]], c(6,4,4))
  corr_mat_v2[i,] <- sapply(1:30, function(x) getCosDistance(full_sig,sig_full_v2[,x+3]))
}
rownames(corr_mat_v2) <- paste0("P", 1:length(Fs))
colnames(corr_mat_v2) <- paste0("C", 1:30)

# cosmic v3
corr_mat_v3 <- matrix(NA, length(Fs), ncol(sig_full_v3)-2)
for(i in 1:length(Fs)){
  full_sig <- convertSignatureMatrixToVector(Fs[[i]], c(6,4,4))
  corr_mat_v3[i,] <- sapply(1:(ncol(sig_full_v3)-2), function(x) getCosDistance(full_sig,unlist(sig_full_v3[,x+2])))
}
rownames(corr_mat_v3) <- paste0("P", 1:length(Fs))
colnames(corr_mat_v3) <- colnames(sig_full_v3)[-c(1:2)]

myCol <- colorRampPalette(c("#F8F8FF", "#F8F8FF", "#F8F8FF", "#6B8E23"))

url_share <- "https://twitter.com/intent/tweet?text=Excited%20to%20share%20this%20Shiny%20app%20with%20you&url=http://www.github.com/USCbiostat/iMutSig"
url_cite <- "http://www.github.com/USCbiostats/iMutSig"
