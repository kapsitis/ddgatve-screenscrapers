#setwd("/home/st/ddgatve-screenscrapers/src/main/r")
setwd("/home/kalvis/workspace/ddgatve-screenscrapers/src/main/r")

if (!"plyr" %in% installed.packages()) install.packages("plyr")
library(plyr)
df <- read.csv("../resources/data-candidates/candidates-all.csv", 
               na.strings="N/A", fileEncoding="UTF-8", strip.white=TRUE)
parties <- read.csv("../resources/data-parties/parties.csv", 
                    na.strings="N/A", fileEncoding="UTF-8", strip.white=TRUE)

saeima12 <-  read.csv("../resources/data-candidates/candidates-pre12.csv", 
                      na.strings="N/A", fileEncoding="UTF-8", strip.white=TRUE)

percentageBlack <- rep(0,times=12)
percentageGreen <- rep(0,times=12)
percentageRed <- rep(0,times=12)
percentageBlue <- rep(0,times=12)
#for (saeima in c(12))
for (saeima in c(6,7,8,9,10,11,12)) {
  png(width = 960, height = 480, pointsize = 16, 
      filename=paste0("women-saeima",saeima,".png"))
  
  leftMargin <- 11
  partyNameShift <- -32
  
  if (saeima == 12) {
    xxNr <- 1:13
    xxPartyAbbr <- unique(saeima12$PartyAbbr)
    xxProcenti <- c(0,0,0,25,0,
                    0,25,0,0,0,
                    25,25,0)
    #     xxProcenti <- c(0,0,0,20,0,
    #                     0,20,0,0,0,
    #                     20,20,20)
    xx <- data.frame(Nr = xxNr, Saeima = 12, Saraksts = xxPartyAbbr, 
                     Saīsinājums  = xxPartyAbbr, Procenti = xxProcenti, 
                     Vietas = xxProcenti, Aploksnes = 1000000)
  } else {  
    xx <-subset(parties, Saeima == saeima)
  }
  
  maleCount = c()
  femaleCount = c()
  sexRatio = c()
  for (num in xx$Nr) {
    if (saeima == 12) {
      mySaraksts <- subset(saeima12, Saeima == 12 & PartyNum == num)
      myTop <- subset(saeima12, Saeima == 12 & PartyNum == num & PreNum <= 5)
    }
    else {
      mySaraksts <- subset(df, Saeima == saeima & PartyNum == num)
      myTop <- subset(df, Saeima == saeima & PartyNum == num & PreNum <= 5)      
    }
    
    uniqueNames <- !duplicated(mySaraksts[,c("CandidateName", "CandidateSurname")])
    myNames <- mySaraksts[uniqueNames,]
    
    uniqueTopNames <- !duplicated(myTop[,c("CandidateName", "CandidateSurname")])
    myTopNames <- myTop[uniqueTopNames,]
    
    xx$MaleCount[num] <- sum(myNames$Sex == "Male")
    xx$FemaleCount[num] <- sum(myNames$Sex == "Female")
    xx$SexRatio[num] <- xx$FemaleCount[num]/(xx$MaleCount[num] + xx$FemaleCount[num])
    xx$MaleTopCount[num] <- sum(myTopNames$Sex == "Male")
    xx$FemaleTopCount[num] <- sum(myTopNames$Sex == "Female")
    xx$SexTopRatio[num] <- xx$FemaleTopCount[num]/(xx$MaleTopCount[num] + xx$FemaleTopCount[num])
    
    if (saeima <12) {
      xx$ElectedMaleCount[num] <- nrow(
        subset(df, Saeima == saeima & Sex == "Male" & (Result == "ievēlēts" | Result == "ievēlēta"))
      ) 
      xx$ElectedFemaleCount[num] <- nrow(
        subset(df, Saeima == saeima & Sex == "Female" & (Result == "ievēlēts" | Result == "ievēlēta"))
      ) 
    }
    print(paste0("Saeima ", saeima, ", Liste ", num, ": ", mySaraksts[1,"PartyAbbr"]))
    print(paste0("  Males are ",xx$MaleCount[num],", females are ",xx$FemaleCount[num]))
  }
  
  
  pp <-subset(df, Saeima == saeima)
  ord <- order(xx$SexRatio,decreasing=TRUE)
  
  # Reorder parties by decreasing percentage of women. 
  xx <- xx[ord,]
  
  counts <- matrix(c(xx$FemaleCount,xx$MaleCount), nrow=2, byrow=TRUE, 
                   dimnames = list(c("Female", "Male"), 1:nrow(xx)))
  par(mar=c(5, leftMargin, 4, 2) + 0.1)
  
  barplot(counts,horiz=TRUE,
          xlab="Kandidātu skaits", col=c("pink","lightblue"),xlim=c(0,115),
          axisnames=FALSE)
  par(xpd=FALSE)
  abline(v=seq(0,100,by=10),col = "lightgray", lty = "dotted")
  relativeWeight <- round(100*sum(xx$FemaleCount)/(sum(xx$FemaleCount)+sum(xx$MaleCount)), digits=1)
  
  LParties <- subset(xx, Procenti >= 5.0 )
  largePartyRelativeWeight <- round(100*sum(LParties$FemaleCount)/(sum(LParties$FemaleCount)+sum(LParties$MaleCount)), digits=1)
  largePartyTopRelativeWeight <- round(100*sum(LParties$FemaleTopCount)/(sum(LParties$FemaleTopCount)+sum(LParties$MaleTopCount)), digits=1)
  
  percentageBlack[saeima] <- relativeWeight
  percentageGreen[saeima] <- largePartyRelativeWeight
  percentageRed[saeima] <- largePartyTopRelativeWeight
  if (saeima < 12) {
    electedWeight <- round(100*sum(xx$ElectedFemaleCount)/(sum(xx$ElectedFemaleCount) + sum(xx$ElectedMaleCount)))      
    percentageBlue[saeima] <- electedWeight
  }
  
  title(sprintf("%s.Saeima: Deputātu kandidāti pēc dzimuma",saeima))
  par(xpd=TRUE)
  legend(-24,0,c("Sievietes", "Vīrieši"), fill=c("pink", "lightblue"))
  text(partyNameShift+10,1.2*nrow(xx)+1.5,
       sprintf("Sieviešu kandidātu: %1.1f%%.",relativeWeight), pos=4)
  text(partyNameShift+45, 1.2*nrow(xx)+1.5,font=2,
       sprintf("Saeimā pārstāvētām partijām: %1.1f%%.",
               largePartyRelativeWeight), pos=4, col="#009900")
  text(partyNameShift+92, 1.2*nrow(xx)+1.5,font=2,
       sprintf("T.sk. pirmajiem 25: %1.1f%%.",
               largePartyTopRelativeWeight), pos=4, col="red")
  
  for (i in 1:nrow(xx)) {
    if (xx$Procenti[i]>=5.0) {
      text(partyNameShift,1.2*i - 0.5,xx$Saīsinājums[i], cex=0.9,pos=4,col="#009900", font=2)
    }
    else {
      text(partyNameShift,1.2*i - 0.5,xx$Saīsinājums[i], cex=0.9,pos=4)
    }
    if (xx$FemaleCount[i] > 0) {
      text(xx$FemaleCount[i]/2, 1.2*i - 0.5,xx$FemaleCount[i], cex=0.7)
    }
    if (xx$MaleCount[i] > 0) {
      text(xx$FemaleCount[i] + xx$MaleCount[i]/2, 1.2*i - 0.5, xx$MaleCount[i], cex=0.7)
    }
  }
  par(xpd=FALSE)
  dev.off()
}



png(width = 960, height = 480, pointsize = 16, 
    filename="women-summary.png")

plot(6:12, percentageBlack[6:12], type="b", 
     lwd=2, col="black",pch=20, ylim=c(5,35), 
     ylab="Sieviešu īpatsvars", xlab="Saeima")
# Paint large parties
points(6:11, percentageGreen[6:11], type="b", 
       lwd=2, col = "#009900", pch=20)
points(6:11, percentageRed[6:11], type="b",
       lwd=2, col="red", pch=20)
points(11:12, c(percentageGreen[11],30), type="b", 
       lwd=2, col = "#009900", pch=20, lty="dashed")
points(11:12, c(percentageRed[11],24), type="b",
       lwd=2, col="red", pch=20, lty="dashed")
points(11:12, percentageGreen[11:12], type="b", 
       lwd=2, col = "#009900", pch=20, lty="dashed")
points(11:12, percentageRed[11:12], type="b",
       lwd=2, col="red", pch=20, lty="dashed")
points(6:11, percentageBlue[6:11], type="b", 
       lwd=2, col = "blue", pch=20)

grid()
par(xpd=TRUE)
legend(10,15,c("Visi kand.", ">=5% partijās", ">=5% partiju pirmie 25","Ievēlēti"), 
       col=c("black", "#009900","red","blue"), lwd=2, pch=20)
par(xpd=FALSE)
dev.off()
