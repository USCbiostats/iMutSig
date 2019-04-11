visPMS_full_modified <- function(Fvec, numBases, trDir) {
  
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
  
  if (trDir == TRUE) {
    X$strand <- factor(c(rep("plus", flankingPatternNum * 6), rep("minus", flankingPatternNum * 6)), levels=c("plus", "minus"))
    X$subtype <- factor(rep(subPattern, 2), levels=c("C>A", "C>G", "C>T", "T>A", "T>C", "T>G"))
    X$flank <- paste0(rep(c("A", "C", "G", "T"), each=4), rep("."), rep(c("A", "C", "G", "T"), 4))
  } else {
    X$subtype <- factor(subPattern, levels=c("C>A", "C>G", "C>T", "T>A", "T>C", "T>G"))
    X$flank <- paste0(rep(c("A", "C", "G", "T"), each=4), rep("."), rep(c("A", "C", "G", "T"), 4))
  }
  
  
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
 
  
  if (trDir == TRUE) {
    gp <- gp + facet_grid(strand ~ subtype)
  } else {
    gp <- gp + facet_grid(. ~ subtype)
  }
  
  gp
  
}


convertSignatureMatrixToVector<- function(Fmat,fdim) {
  
  if(nrow(Fmat)<length(fdim) | (nrow(Fmat)==5 & length(fdim)==4)){ 
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