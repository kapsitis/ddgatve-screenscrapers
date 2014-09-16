#setwd("/home/st/ddgatve-screenscrapers/src/main/r")
setwd("/home/kalvis/workspace/ddgatve-screenscrapers/src/main/r")

if (!"plyr" %in% installed.packages()) install.packages("plyr")
if (!"plotrix" %in% installed.packages()) install.packages("plotrix")

df <- read.csv("../resources/data-candidates/candidates-all.csv", 
               fileEncoding="UTF-8", strip.white=TRUE)
parties <- read.csv("../resources/data-parties/parties.csv", 
                    na.strings="N/A", fileEncoding="UTF-8", strip.white=TRUE)

library(plyr)
library(plotrix)

# myDF <- subset(df, Saeima == mySaeima)


distrs <- rep(c("R","V","L","K","Z"), times=5)


for (saeima in c(10,11)) { 
  
    png(width = 960, height = 480, pointsize = 16, 
        filename=paste0("boxplot-saeima",saeima,".png"))
  

  naColor <- "#DAA520"
  vienotibaColor <- "#7fff00"
  scColor <- "#FF0B08"
  zrpColor <- "#00BADF"
  pllColor <- "#A020F0"
  zzsColor <- "#009535"
  if (saeima == 11) {
    colorList <- c(vienotibaColor,zrpColor, scColor, naColor, zzsColor)
    abbrList <- c("Vien.","ZRP","SC","NA","ZZS")
  }
  if (saeima == 10) {   
    colorList <- c(vienotibaColor,scColor, zzsColor, pllColor, naColor)
    abbrList <- c("Vien.","SC","ZZS","PLL","NA")
  }
  
  
  lastElected <- c()
  countElected <- c()
  partiesForSaeima <- subset(parties, Saeima == saeima & Procenti >= 5.0)
  theNumbers <- partiesForSaeima$Nr
  theData <- vector(mode="list", length=5*length(theNumbers))
  
  for (idx in 1:length(theNumbers)) {
    myNumber <- theNumbers[idx]
    for (i in 1:5) {
      myDF <- subset(df, Saeima == saeima & PartyNum == myNumber & DistrNum == i)
      myDF$Elected <- myDF$Result == "ievēlēts" | myDF$Result == "ievēlēta" 
      
      myDF$RelPoints <- (myDF$Pluses - myDF$Minuses)/myDF$BallotsForParty
      lastElected[5*(idx-1) + i] <- min(myDF$RelPoints[myDF$Elected])
      theData[[5*(idx-1) + i]] <- myDF$RelPoints
      countElected[[5*(idx-1) + i]] <- length(myDF$RelPoints[myDF$Elected])
    } 
  }
  xLower <- -0.2
  xUpper <- 0.8
  gridStep <- 0.1
  
  boxHeights <- rev(1:length(theData) + rep(1:5,each=5)/2)
  boxplot(theData, horizontal=TRUE, outline=TRUE, 
          border=rep(colorList,each=5), names=distrs, range=0, axes=FALSE,
          at=boxHeights)
  
  partyNames <- as.vector(partiesForSaeima$Saīsinājums)
  for (i in 1:length(lastElected)) {
    borderColor <- "#CC0033"
    if (partyNames[(i+4) %/% 5] == "SC") { borderColor <- "blue" }
    draw.circle(lastElected[i], boxHeights[i], 0.005, 
                col = "white", lwd=2, border = borderColor)
  }
  
  axis(1, seq(xLower,xUpper,by=gridStep), 
       paste0(round(100*seq(xLower,xUpper,by=gridStep)),"%"))
  vGrid <- seq(xLower,xUpper,by=gridStep)
  abline(v=vGrid[!vGrid==0], col = "lightgray", lty = "dotted")
  abline(v=0, col = "red", lty = "dotted")
  title(paste0(saeima, ".Saeimas kandidātu punkti: Kastīšu-ūsu diagramma"), 
               sub="Plusi - Svītrojumi (relatīvi pret saraksta balsu kopskaitu apgabalā)")
  
  
  par(xpd=TRUE,srt=90)
  
  
  for (cc in 1:length(colorList)) {
    rect(-0.25, boxHeights[cc*5 - 4]+0.5, -0.2, boxHeights[cc*5]-0.5, 
         col = colorList[cc], border = colorList[cc])
    textHeight <- (boxHeights[cc*5 - 4]+boxHeights[cc*5])/2
    text(-0.215,textHeight-0.5,abbrList[cc], cex=1.4, pos=3,font=2,col="white") 
  }
  par(srt=0)
  for (hh in 1:length(boxHeights)) {
    text(-0.19 + 0.01*((hh-1) %% 5),boxHeights[hh],distrs[hh], cex=0.8, pos=4,col="black")
    text(0.72,boxHeights[hh],countElected[hh], cex=0.8, pos=4,col="black")    
  }
  
  par(xpd=FALSE)
  
  dev.off()
}


