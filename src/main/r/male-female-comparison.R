#setwd("/home/st/ddgatve-screenscrapers/src/main/r")
setwd("/home/kalvis/workspace/ddgatve-screenscrapers/src/main/r")

if (!"plyr" %in% installed.packages()) install.packages("plyr")
library(plyr)
df <- read.csv("../resources/data-candidates/candidates-all.csv", 
               na.strings="N/A", fileEncoding="UTF-8", strip.white=TRUE)
parties <- read.csv("../resources/data-parties/parties.csv", 
                    na.strings="N/A", fileEncoding="UTF-8", strip.white=TRUE)

saeima <- 11
leftMargin <- 11
if (saeima == 6) {
  partyNameShift <- -40
  legendLeft <- 75
}
else {
  partyNameShift <- -30
  legendLeft <- 65
}



partiesForSaeima <-subset(parties, Saeima == saeima)
#  xx <- partiesForSaeima[partiesForSaeima$Procenti >= 2.0,]
xx <- partiesForSaeima
bigPartyNumbers <- xx[,"Nr"]
maleCount = c()
femaleCount = c()
sexRatio = c()
for (num in bigPartyNumbers) {
  mySaraksts <- subset(df, Saeima == saeima & PartyNum == num)
  uniqueNames <- !duplicated(mySaraksts[,c("CandidateName", "CandidateSurname")])
  myNames <- mySaraksts[uniqueNames,]
  maleCount[num] <- nrow(myNames[myNames$Sex == "Male",])
  femaleCount[num] <- nrow(myNames[myNames$Sex == "Female",])
  sexRatio[num] <- femaleCount[num]/maleCount[num]
  sexualCount[num] = maleCount+femaleCount
  print(paste0("Saeima ", saeima, ", Liste ", num, ": ", mySaraksts[1,"PartyAbbr"]))
  print(paste0("  Males are ",maleCount[num],", females are ",femaleCount[num]))
}


pp <-subset(df, Saeima == saeima)
ord <- order(sexRatio,decreasing=TRUE)
pNames <- parties[parties$Saeima==saeima,"Saīsinājums"][ord]
pPercentage <- parties[parties$Saeima==saeima, "Procenti"][ord]
pMalesReordered <- maleCount[ord]
pFemalesReordered <- femaleCount[ord]

counts <- matrix(c(pFemalesReordered,pMalesReordered), nrow=2, byrow=TRUE, 
                 dimnames = list(c("Female", "Male"), 1:length(pNames)))
par(mar=c(5, leftMargin, 4, 2) + 0.1)
barplot(counts,horiz=TRUE,
        xlab="Kandidātu skaits", col=c("pink","lightblue"),
        axisnames=FALSE)
par(xpd=FALSE)
abline(v=seq(0,100,by=10),col = "lightgray", lty = "dotted")
relativeWeight <- round(100*sum(pFemalesReordered)/(sum(pFemalesReordered)+sum(pMalesReordered)), digits=1)

largePartyMales <- sum(pMalesReordered*(pPercentage >= 5.0))
largePartyFemales <- sum(pFemalesReordered*(pPercentage >= 5.0))
largePartyRelativeWeight <- round(100*largePartyFemales/(largePartyMales+largePartyFemales), digits=1)

title(sprintf("%s.Saeima: Deputātu kandidāti pēc dzimuma",saeima))
par(xpd=TRUE)
legend(legendLeft,5,c("Sievietes", "Vīrieši"), fill=c("pink", "lightblue"))
text(partyNameShift+10,1.2*length(pNames)+1.5,
     sprintf("Sieviešu kandidātu: %1.1f%%",relativeWeight), pos=4)
text(partyNameShift+50, 1.2*length(pNames)+1.5,font=2,
     sprintf("(starp Saeimā pārstāvētām partijām: %1.1f%%)",largePartyRelativeWeight), pos=4, col="#009900")
for (i in 1:length(pNames)) {
  if (pPercentage[i]>=5.0) {
    text(partyNameShift,1.2*i - 0.5,pNames[i], cex=0.9,pos=4,col="#009900", font=2)
  }
  else {
    text(partyNameShift,1.2*i - 0.5,pNames[i], cex=0.9,pos=4)
  }
  if (pFemalesReordered[i] > 0) {
    text(pFemalesReordered[i]/2, 1.2*i - 0.5,pFemalesReordered[i], cex=0.7)
  }
  if (pMalesReordered[i] > 0) {
    text(pFemalesReordered[i] + pMalesReordered[i]/2, 1.2*i - 0.5,pMalesReordered[i], cex=0.7)
  }
}



