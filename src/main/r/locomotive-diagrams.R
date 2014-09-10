#setwd("/home/st/ddgatve-screenscrapers/src/main/r")
setwd("/home/kalvis/workspace/ddgatve-screenscrapers/src/main/r")

if (!"plyr" %in% installed.packages()) install.packages("plyr")
# candidateFiles = c("candidates06.csv", "candidates07.csv", "candidates08.csv",
#                    "candidates09.csv", "candidates10.csv", "candidates11.csv")

df <- read.csv(paste0("../resources/data-candidates/",
                      "candidates-all.csv"), fileEncoding="UTF-8", strip.white=TRUE)

library(plyr)

mySaeima <- "6"
myNumber <- "9"

mySaraksts = subset(df, Saeima == mySaeima & PartyNum == myNumber)

points <- mySaraksts[,"Points"]
ballots <- mySaraksts[,"BallotsForParty"] 
relativeX <- (points-ballots)/ballots

distrNum <- mySaraksts[,"DistrNum"]
elected <- mySaraksts[,"Result"]=="ievēlēts" | mySaraksts[,"Result"]=="ievēlēta" 
nonElected <- mySaraksts[,"Result"]!="ievēlēts" & mySaraksts[,"Result"]!="ievēlēta"

xLower <- -0.20
xUpper <- 0.40
gridStep <- 0.05

xx <- c(xLower,xUpper)
yy <- c(1,5.5)
# novadi <- c("R", "V", "L", "K", "Z")
# novadi <- c("Z","K","L","V","R")
novadiNames <- c("Rīga", "Vidzeme", "Latgale", "Kurzeme", "Zemgale")

plot(xx,yy, type="n", axes = FALSE,
     xlab="",ylab="")

numElected <- c()
gaps <- c()
# Compute for each district separately:
for (i in 1:5) {
  dxx <- relativeX[which(distrNum == i)]
  dyy <- rep(6-i,length(which(distrNum == i)))
  dPoints <- sort(points[which(distrNum == i)], decreasing=TRUE)
  dElected <- which(elected[which(distrNum == i)])
  dNonelected <- which(nonElected[which(distrNum == i)])
  if (length(dElected)>0) {
    Candidate0 <- max(dElected)
  }
  else {
    Candidate0 <- 0
  }
  numElected[i] <- Candidate0
  Candidate1 <- min(dNonelected)
#  print(paste0("i is", i, "maxelected is ", Candidate0, ", minNonelected is ", Candidate1))
  median <- fivenum(dxx)[3]
  shiftedDxx <- dxx - median
  points(shiftedDxx,dyy)
  
  sortedDxx <- sort(shiftedDxx,decreasing=TRUE)
  
#  print(paste0("  sortedDxx is ", sortedDxx))
  if (Candidate0 > 0) {
    leftSide <- sortedDxx[Candidate1]
    rightSide <- sortedDxx[Candidate0]
    midpoint <- (leftSide + rightSide)/2
   segments(x0 <- midpoint, y0 <- (6-i) - 0.1, x1 <- midpoint, y1 <- (6-i) + 0.2, col="red", lwd=2)
   arrows(x0 <- midpoint, y0 <- (6-i) + 0.2, x1 <- midpoint+0.05, 
          col="red", lwd=2, length = 0.10, angle = 15)
   pointDifference <- dPoints[Candidate0] - dPoints[Candidate1]
   text(midpoint,6- i + 0.35, pointDifference, col="red", cex=0.7)
  }
  else {
    maxVal <- max(sortedDxx)
    symbols(x=maxVal+gridStep*0.4, y=6-i+0.2, circles=0.02, fg="red",lwd=1, add=TRUE, inches=FALSE)
  }  
}
title(paste0(mySaeima, ".Saeima, #", myNumber, ": ", 
             mySaraksts[1,"PartyAbbr"],"\nievēlēti ",sum(numElected)))
axis(1, seq(xLower,xUpper,by=gridStep), seq(xLower,xUpper,by=gridStep))
# axis(2, 1:5, novadi)


leftmostGridline <- gridStep*(floor(xLower/gridStep)+1)
rightmostGridline <- gridStep*floor(xUpper/gridStep)

#text(rep(leftmostGridline - gridStep/2,5),5:1 + 0.2, paste0(novadiNames,"(",numElected,")"))
text(rep(xLower,5),5:1 + 0.2, paste0(novadiNames,"(",numElected,")"), pos=4)

abline(h=1:5, col = "lightgray", lty = "dotted")
abline(v=seq(leftmostGridline,rightmostGridline,by=gridStep), col = "lightgray", lty = "dotted")
#grid()
# mySaraksts[,"PartyAbbr"]
