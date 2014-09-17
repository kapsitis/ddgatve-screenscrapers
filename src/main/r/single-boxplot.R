setwd("/home/st/ddgatve-screenscrapers/src/main/r")
#setwd("/home/kalvis/workspace/ddgatve-screenscrapers/src/main/r")

if (!"plyr" %in% installed.packages()) install.packages("plyr")
if (!"plotrix" %in% installed.packages()) install.packages("plotrix")

df <- read.csv("../resources/data-candidates/candidates-all.csv", 
               fileEncoding="UTF-8", strip.white=TRUE)

library(plyr)
library(plotrix)


saeima <- 11
distrNum <- 3
myNumber <- 3

# Generage two pictures: (1) with points and candidates; with IQR explained
for (run in 2) {
  png(width = 960, height = 480, pointsize = 16, 
      filename=paste0("single-boxplot-", run, ".png"))
  
  vienotibaColor <- "#7fff00"
  
  lastElected <- c()
  countElected <- c()
  theData <- vector(mode="list", length=1)
  
  myDF <- subset(df, Saeima == saeima & PartyNum == myNumber & DistrNum == distrNum)
  myDF$Elected <- myDF$Result == "ievēlēts" | myDF$Result == "ievēlēta" 
  
  myDF$RelPoints <- (myDF$Pluses - myDF$Minuses)/myDF$BallotsForParty
  lastElected <- min(myDF$RelPoints[myDF$Elected])
  theData <- myDF$Points
  countElected <- length(myDF$RelPoints[myDF$Elected])
  
  xLower <- 44000
  xUpper <- 60000
  gridStep <- 2000
  
  boxplot(theData, horizontal=TRUE, outline=TRUE, 
          border="#00CC00", names="", range=0, 
          at=1, lwd=2, axes=FALSE)
  vGrid <- seq(xLower,xUpper,by=gridStep)
  axis(1, vGrid, vGrid)
  abline(v=vGrid, col = "lightgray", lty = "dotted")
  abline(v=myDF$BallotsForParty[1], col = "blue", lty="dotted")
  
  if (run == 1) {
    par(xpd=TRUE)
    text(myDF$BallotsForParty[1],0.2,"Par sarakstu nobalsojušo skaits", 
         cex=1, pos=3, col="blue") 
    text(myDF$BallotsForParty[1],0.13,myDF$BallotsForParty[1], 
         cex=1, pos=3, col="blue") 
    par(xpd=FALSE)
    
    
  }
  
  for (idx in 1:length(myDF$Points)) {
    if (myDF$Elected[idx]) {
      draw.circle(x=myDF$Points[idx], y=1, radius=150, col = "white", lwd=2, border = "red")
    }
    else {
      draw.circle(x=myDF$Points[idx], y=1, radius=150, col = "white", lwd=1, border = "black")
    }
  }
  
  title(paste0(saeima, ".Saeima; #1 (Vienotība) Rīgā",
               "\nKandidātu punkti = Balsis Apgabalā + Plusi - Svītrojumi"))
  
  dev.off()
}

