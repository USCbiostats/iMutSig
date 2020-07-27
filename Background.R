###################
## Define functions 
###################

visPMS_full_modified <- function(Fvec, numBases) {
  
  flankingPatternNum <- 4^(numBases - 1)
  subPattern <- c(rep("C>A", flankingPatternNum), 
                  rep("C>G", flankingPatternNum), 
                  rep("C>T", flankingPatternNum), 
                  rep("T>A", flankingPatternNum), 
                  rep("T>C", flankingPatternNum), 
                  rep("T>G", flankingPatternNum)
  )
  
  X <- data.frame(probability = Fvec)
  # X$strand <- factor(rep(c("plus", "minus"), 1536), levels=c("plus", "minus"))
  X$subtype <- factor(subPattern, levels=c("C>A", "C>G", "C>T", "T>A", "T>C", "T>G"))
  X$flank <- paste0(rep(c("A", "C", "G", "T"), each=4), rep("."), rep(c("A", "C", "G", "T"), 4))
  
  gp <- ggplot(X, aes(x=flank, y=probability, fill=subtype)) +
    geom_bar(stat="identity", position="identity") + 
    theme_bw() + 
    theme(axis.text.x = element_text(face="bold", color="grey",size=5, 
                                     angle=90, vjust=0.5),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=rel(1.2)),
          axis.title.y = element_text(size=rel(1.2)),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          strip.text= element_text(face="bold", size=rel(1.2))) +
    guides(fill=FALSE) 
  
  gp <- gp + facet_grid(. ~ subtype)
  
  gp
  
}


convertSignatureMatrixToVector<- function(Fmat,fdim) {
  
  if(nrow(Fmat)<length(fdim)){ 
    stop("The signature matrix does not have enough information!")
  } else if(nrow(Fmat)>4 & length(fdim)==3){
    index <- c(1,3,4)
  } else if(nrow(Fmat)==6 & length(fdim)==4) {
    index <- c(1,3,4,6)
  } else {
    index <- 1:length(fdim)
  }
  
  
  Fmat <- Fmat[index,]
  M <- prod(fdim)
  Fvec <- rep(1, M)
  Names <- rep("", M)
  
  
  temp1 <- 1
  temp2 <- 1
  for (i in 1:length(fdim)) {
    temp1 <- temp1 * fdim[i]
    divInd <- (1:M - 1) %% temp1 + 1
    for (j in 1:fdim[i]) {
      targetInd <- divInd > temp2 * (j - 1) & divInd <= temp2 * j
      Fvec[targetInd] <- Fvec[targetInd] * Fmat[i,j]
      Names[targetInd] <- paste(Names[targetInd],j,sep="") 
    }
    temp2 <- temp2 * fdim[i]
  }
  
  num=as.numeric(Names)
  num=num[order(as.numeric(Names))]
  test=Fvec[order(as.numeric(Names))]
  
  return(test)
  
}


getCosDistance <- function(F_1, F_2) {
  
  if(length(F_1)!=length(F_2)){
    geterrmessage("Two signatures have different number of bases!")
  }
  
  return(sum(F_1/sqrt(sum(F_1^2))*F_2/sqrt(sum(F_2^2))))
  
}


########################################################################################
## This part needs to be manually updated whenever there is a new update on signatures. 
########################################################################################

Update <- FALSE
if (Update){
  download.file("https://cancer.sanger.ac.uk/cancergenome/assets/signatures_probabilities.txt",
                "inst/extdata/sig_v2.txt", mode = 'wb')
  sig_file_v2 <- read.delim("inst/extdata/sig_v2.txt") 
  sig_full_v2 <- sig_file_v2[order(sig_file_v2[,1]),1:33]
  
  download.file("https://cancer.sanger.ac.uk/signatures/COSMIC_Mutational_Signatures_v3.1.xlsx",
                "inst/extdata/sig_v3.1.xlsx", mode = 'wb')
  sig_file_v3 <- readxl::read_excel("inst/extdata/sig_v3.1.xlsx") 
  sig_full_v3 <- sig_file_v3[order(sig_file_v3[,1]),]
  sig_full_v3 <- data.frame(sig_full_v3)
  
  save(sig_full_v2, sig_full_v3, file="inst/extdata/COSMIC_sig.rdata")
  
  pm_corr <- read.csv("inst/extdata/pm_corr.csv", na = "0") %>% as.matrix()
  pm_corr[is.na(pm_corr)] <- 0
  rownames(pm_corr) <- paste0("P", 1:27)
  
  # page 1, v2
  cosmic_corr_v2 <- read.csv("inst/extdata/cosmic_corr.csv", na = "0") %>% as.matrix()
  cosmic_corr_v2[is.na(cosmic_corr_v2)] <- 0
  rownames(cosmic_corr_v2) <- c(paste0("C", 1:30), "Other")
  
  # page 1, v3
  cosmic_corr_v3_raw <- read.csv("inst/extdata/PCAWG_sigProfiler_SBS_signatures_in_samples.csv", header = TRUE)
  cosmic_corr_v3 <- sapply(unique(cosmic_corr_v3_raw$Cancer.Types), function(x){
    cosmic_corr_v3_raw %>% filter(Cancer.Types==x) %>% select(-c(1:3)) %>% sapply(., function(y) mean(y!=0))
  })
  colnames(cosmic_corr_v3) <- unique(cosmic_corr_v3_raw$Cancer.Types)
  missingSig <- setdiff(colnames(sig_full_v3)[-c(1:2)], rownames(cosmic_corr_v3))
  for(i in seq_along(missingSig)){
    cosmic_corr_v3 <- rbind(cosmic_corr_v3, 0)
    rownames(cosmic_corr_v3)[nrow(cosmic_corr_v3)] <- missingSig[i]
  }
  
  ### cosmic v2, expand
  corr_mat_v2_exp <- matrix(NA, length(Fs), 30)
  for(i in 1:length(Fs)){
    full_sig <- convertSignatureMatrixToVector(Fs[[i]], c(6,4,4))
    corr_mat_v2_exp[i,] <- sapply(1:30, function(x) getCosDistance(full_sig,sig_full_v2[,x+3]))
  }
  rownames(corr_mat_v2_exp) <- paste0("P", 1:length(Fs))
  colnames(corr_mat_v2_exp) <- paste0("C", 1:30)
  
  ### cosmic v2, collapse
  corr_mat_v2_col <- matrix(NA, length(Fs), 30)
  for(i in 1:length(Fs)){
    sig_full_v2_col <- lapply(1:30, function(x) decompTumor2Sig::convertAlexandrov2Shiraishi(sig_full_v2[, x+3])[[1]])
    corr_mat_v2_col[i,] <- sapply(1:30, function(x) getCosDistance(c(Fs[[i]][c(1,3,4),])[-c(14,15,17,18)], 
                                                                   c(sig_full_v2_col[[x]])[-c(14,15,17,18)]))
  }
  rownames(corr_mat_v2_col) <- paste0("P", 1:length(Fs))
  colnames(corr_mat_v2_col) <- paste0("C", 1:30)
  
  corr_mat_v2 <- list("Expand" = corr_mat_v2_exp,
                      "Collapse" = corr_mat_v2_col)
  
  ### cosmic v3, expand
  corr_mat_v3_exp <- matrix(NA, length(Fs), ncol(sig_full_v3)-2)
  for(i in 1:length(Fs)){
    full_sig <- convertSignatureMatrixToVector(Fs[[i]], c(6,4,4))
    corr_mat_v3_exp[i,] <- sapply(1:(ncol(sig_full_v3)-2), function(x) getCosDistance(full_sig,unlist(sig_full_v3[,x+2])))
  }
  rownames(corr_mat_v3_exp) <- paste0("P", 1:length(Fs))
  colnames(corr_mat_v3_exp) <- colnames(sig_full_v3)[-c(1:2)]
  
  ### cosmic v3, collapse
  corr_mat_v3_col <- matrix(NA, length(Fs), ncol(sig_full_v3)-2)
  for(i in 1:length(Fs)){
    sig_full_v3_col <- lapply(1:(ncol(sig_full_v3)-2), function(x) decompTumor2Sig::convertAlexandrov2Shiraishi(sig_full_v3[, x+2])[[1]])
    corr_mat_v3_col[i,] <- sapply(1:(ncol(sig_full_v3)-2), function(x) getCosDistance(c(Fs[[i]][c(1,3,4),])[-c(14,15,17,18)], 
                                                                                      c(sig_full_v3_col[[x]])[-c(14,15,17,18)]))
  }
  rownames(corr_mat_v3_col) <- paste0("P", 1:length(Fs))
  colnames(corr_mat_v3_col) <- colnames(sig_full_v3)[-c(1:2)]
  
  corr_mat_v3 <- list("Expand" = corr_mat_v3_exp,
                      "Collapse" = corr_mat_v3_col)
  
  save(corr_mat_v2, corr_mat_v3, pm_corr, cosmic_corr_v2, cosmic_corr_v3, file="inst/extdata/corr_mat.rdata")
}

load("inst/extdata/Signaturelog.RData")
load("inst/extdata/COSMIC_sig.rdata")
load("inst/extdata/corr_mat.rdata")

myCol <- colorRampPalette(c("#F8F8FF", "#F8F8FF", "#F8F8FF", "#6B8E23"))

url_share <- "https://twitter.com/intent/tweet?text=Excited%20to%20share%20this%20Shiny%20app%20with%20you&url=http://www.github.com/USCbiostat/iMutSig"
url_cite <- "http://www.github.com/USCbiostats/iMutSig"
