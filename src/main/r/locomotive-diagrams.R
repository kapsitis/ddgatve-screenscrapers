#setwd("/home/st/ddgatve-screenscrapers/src/main/r")
setwd("/home/kalvis/workspace/ddgatve-screenscrapers/src/main/r")

if (!"plyr" %in% installed.packages()) install.packages("plyr")

df <- read.csv("../resources/data-candidates/candidates-all.csv", 
                      fileEncoding="UTF-8", strip.white=TRUE)
parties <- read.csv("../resources/data-parties/parties.csv", 
                    na.strings="N/A", fileEncoding="UTF-8", strip.white=TRUE)

library(plyr)


getMessage <- function(a,b,c) {
  return(paste0(substring(a,1,1),".",b,"(",round(100*c),"%)"))
}

mySaeima <- "10"
# Get numbers of all parties represented in Saeima "mySaeima"
partiesForSaeima <-subset(parties, Saeima == mySaeima & Procenti >= 5.0)
theNumbers <- partiesForSaeima$Nr

# Draw diagram with current saeima and current party number

for (myNumber in theNumbers) {
  png(width = 960, height = 480, pointsize = 16, 
      filename=paste0("locomotives-saeima",mySaeima,"-",myNumber,".png"))
  
  myDF <- subset(df, Saeima == mySaeima & PartyNum == myNumber)
  myDF$Elected <- myDF$Result == "ievēlēts" | myDF$Result == "ievēlēta" 
  myDF$RelX <- (myDF$Points - myDF$BallotsForParty)/myDF$BallotsForParty
  
  xLower <- -0.15
  xUpper <- 0.30
  gridStep <- 0.05
  
  novadiNames <- c("Rīga", "Vidzeme", "Latgale", "Kurzeme", "Zemgale")
  par(col.sub="blue")
  plot(c(xLower,xUpper),c(1,5.5), type="n", axes = FALSE, xlab="Plusotāju/svītrotāju procents",ylab="")
  
  numElected <- c()
  
  # Compute for each district separately:
  message <- ""
  distrLetters <- c("R","V","L","K","Z")
  avgRedGap = c()
  avgBlueGap = c()
  
  for (i in 1:5) {        
    distDF <- myDF[myDF$DistrNum == i,]
    heights <- rep(6-i,nrow(distDF))
    distDF <- distDF[order(-distDF$Points),]  
    numElected[i] <- sum(distDF$Elected)
    RelXMedian <- fivenum(distDF$RelX)[3]
    points(distDF$RelX - RelXMedian,heights)
    if (max(distDF$RelX) > xUpper) {
      text(xUpper + 0.01, 6-i + 0.25, paste0("(",  distrLetters[i], ")"), col="blue", cex=1)
      message <- paste0(message, "    (", distrLetters[i],"): ")
      for (idx in which(distDF$RelX > xUpper)) {
        message <- paste0(message, getMessage(distDF[idx,"CandidateName"], 
                                              distDF[idx,"CandidateSurname"],
                                              distDF[idx,"RelX"]))
      }
    }
    
    if (numElected[i] > 0) {
      leftSide <- distDF$RelX[numElected[i]]
      redPoint <- leftSide - RelXMedian
      segments(x0 <- redPoint, y0 <- (6-i) - 0.1, 
               x1 <- redPoint, y1 <- (6-i) + 0.1, col="red", lwd=2)
      arrows(x0 <- redPoint, y0 <- (6-i) + 0.1, x1 <- redPoint+0.05, 
             col="red", lwd=2, length = 0.10, angle = 15)
      segments(x0 <- 0, y0 <- (6-i) + 0.4, 
               x1 <- redPoint, y1 <- (6-i) + 0.4, col="blue", lwd=1)
      
      
      distDF$Points
      redDifference <- distDF$Points[numElected[i]] - distDF$Points[numElected[i]+1]
      avgRedGap[i] <- redDifference
      blueDifference <- round(distDF$Points[numElected[i]] - fivenum(distDF$Points)[3])
      avgBlueGap[i] <- blueDifference
#       text(redPoint,6- i + 0.25, redDifference, col="red", cex=0.7)
      text(redPoint/2, 6- i + 0.5, blueDifference, col="blue", cex=0.7)
    }
    else {
      maxVal <- max(distDF$RelX) - RelXMedian
      symbols(x=maxVal+gridStep*0.4, y=6-i+0.2, circles=0.01, fg="red",lwd=1, add=TRUE, inches=FALSE)
    }
  }
  
  
  title(paste0(mySaeima, ".Saeima, #", myNumber, ": ", 
               myDF[1,"PartyAbbr"],"\nIevēlēti ",sum(myDF$Elected)), sub=message)
  axis(1, seq(xLower,xUpper,by=gridStep), 
       paste0(round(100*seq(xLower,xUpper,by=gridStep)),"%"))
  
  
  leftmostGridline <- gridStep*(floor(xLower/gridStep)+1)
  rightmostGridline <- gridStep*floor(xUpper/gridStep)
  
  text(rep(xLower,5),5:1 + 0.2, paste0(novadiNames,"(",numElected,")"), pos=4)
  
  abline(h=1:5, col = "lightgray", lty = "dotted")
  vGrid <- seq(leftmostGridline,rightmostGridline,by=gridStep)
  abline(v=vGrid[!vGrid==0], col = "lightgray", lty = "dotted")
  abline(v=0, col = "red", lty = "dotted")
  
  par(xpd=TRUE)
#   text(-0.14,6.4, paste0("Avg.Red gap ",mean(avgRedGap)), pos=4, col="red")
  text(-0.14,6, paste0("Vid.indiv.kampaņa ",round(mean(avgBlueGap))), pos=4, col="blue")
  
  dev.off()
}

