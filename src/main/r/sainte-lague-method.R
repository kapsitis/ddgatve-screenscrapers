setwd("/home/st/ddgatve-screenscrapers/src/main/r")
#setwd("/home/kalvis/workspace/ddgatve-screenscrapers/src/main/r")

if (!"plyr" %in% installed.packages()) install.packages("plyr")
# candidateFiles = c("candidates06.csv", "candidates07.csv", "candidates08.csv",
#                    "candidates09.csv", "candidates10.csv", "candidates11.csv")

df <- read.csv("../resources/data-candidates/candidates-all.csv",                      
               na.strings="N/A", fileEncoding="UTF-8", strip.white=TRUE)

parties <- read.csv("../resources/data-parties/parties.csv", 
                    na.strings="N/A", fileEncoding="UTF-8", strip.white=TRUE)


library(plyr)

saeima <- 11
distrNum <- 5

# xx <- partiesForSaeima[partiesForSaeima$Procenti >= 2.0,]

partiesForSaeima <-subset(parties, Saeima == saeima & Procenti >= 5.0)
theNumbers <- partiesForSaeima[,"Nr"]


mySaraksts = subset(df, Saeima == saeima & 
                      PartyNum %in% theNumbers &
                      DistrNum == distrNum)

if (exists("SainteLagueSeq")) {
  rm(SainteLagueSeq)
}

for (num in theNumbers) {
  tempDF <- subset(mySaraksts, PartyNum == num)
  tempDF$slFractions <- 
    tempDF$BallotsForParty/seq(1, by=2, length.out=nrow(tempDF))
  if (!exists("SainteLagueSeq")) {
    SainteLagueSeq <- tempDF
  } 
  else {
    SainteLagueSeq <- rbind(SainteLagueSeq, tempDF)
  }
}

theOrder <- order(SainteLagueSeq$slFractions, 
                  decreasing=TRUE)
commonList <- SainteLagueSeq[theOrder, c("CandidateName", "CandidateSurname", "PartyAbbr")]

# Print first 15 people that are elected according to 
# the Sainte Lague method. 
commonList[1:15,]


