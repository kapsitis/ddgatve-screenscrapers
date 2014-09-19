#setwd("/home/st/ddgatve-screenscrapers/src/main/r")
setwd("/home/kalvis/workspace/ddgatve-screenscrapers/src/main/r")

if (!"plyr" %in% installed.packages()) install.packages("plyr")
if (!"plotrix" %in% installed.packages()) install.packages("plotrix")

df <- read.csv("../resources/data-candidates/candidates-all.csv", 
               fileEncoding="UTF-8", strip.white=TRUE)
library(plyr)
library(plotrix)

saeima <- 11
showBallotNumber <- FALSE


myNumber <- c(1,3,6,8,11)
distrNum <- 1
theTitle <- c("11.Saeima; #1 (Vienotība) Rīgā", "11.Saeima; #3 (ZRP) Rīgā",
              "11.Saeima; #6 (SC) Rīgā", "11.Saeima; #8 (NA) Rīgā",
              "11.Saeima; #11 (ZZS) Rīgā")

naColor <- "#DAA520"
vienotibaColor <- "#00CC00"
#scColor <- "#FF0B08"
scColor <- "blue"
zrpColor <- "#00BADF"
pllColor <- "#A020F0"
zzsColor <- "#009535"
colorList <- rep("black",13)
colorList[1] <- vienotibaColor
colorList[3] <- zrpColor
colorList[6] <- scColor
colorList[8] <- naColor
colorList[11] <- zzsColor


for (idx in 1:length(myNumber)) {
  png(width = 960, height = 480, pointsize = 16, 
      filename=paste0("single-boxplot-riga-", myNumber[idx], ".png"))
  
  theData <- vector(mode="list", length=1)
  
  myDF <- subset(df, Saeima == saeima & PartyNum == myNumber[idx] & DistrNum == distrNum)
  myDF$Elected <- myDF$Result == "ievēlēts" | myDF$Result == "ievēlēta" 
  theData <- myDF$Points
  
  allGridSteps <- c(100,200,500,1000,2000,5000,10000,20000,50000,100000)
  gridRatios <- (max(theData) - min(theData))/allGridSteps
  gridStep <- allGridSteps[min(which(gridRatios <= 8))]
  xLower <- floor(min(c(theData,myDF$BallotsForParty[1]))/gridStep)*gridStep
  xUpper <- ceiling(max(c(theData,myDF$BallotsForParty[1]))/gridStep)*gridStep
  
  
  theRadius <- (max(theData) - min(theData))/100
  
  boxplot(c(myDF$BallotsForParty[1],theData), horizontal=TRUE, outline=TRUE, 
          border="white", names="", range=0, 
          at=1, lwd=2, axes=FALSE)
  
  boxplot(theData, horizontal=TRUE, outline=TRUE, 
          border=colorList[myNumber[idx]], names="", range=0, 
          at=1, lwd=2, axes=FALSE, add=TRUE)
  vGrid <- seq(xLower,xUpper,by=gridStep)
  axis(1, vGrid, vGrid)
  abline(v=vGrid, col = "lightgray", lty = "dotted")
  abline(v=myDF$BallotsForParty[1], col = "blue", lwd=2, lty="dashed")
  
  if (showBallotNumber) {
    par(xpd=TRUE)
    text(myDF$BallotsForParty[1],0.2,"Par sarakstu nobalsojušo skaits", 
         cex=1, pos=3, col="blue") 
    text(myDF$BallotsForParty[1],0.13,myDF$BallotsForParty[1], 
         cex=1, pos=3, col="blue") 
    par(xpd=FALSE)
  }
  
  for (cand in 1:length(myDF$Points)) {
    # elected MPs get a bold red circle
    if (myDF$Elected[cand]) {
      draw.circle(x=myDF$Points[cand], y=1, radius=theRadius, 
                  col = "white", lwd=2, border = "red")
    }
    # all other candidates get a plain black circle
    else {
      draw.circle(x=myDF$Points[cand], y=1, radius=theRadius, 
                  col = "white", lwd=1, border = "black")
    }
  }
  
  title(paste0(theTitle[idx],"\nKandidātu punkti = Balsis Apgabalā + Plusi - Svītrojumi"))
  
  dev.off()
}

