setwd("/home/st/ddgatve-screenscrapers/src/main/r")
#setwd("/home/kalvis/workspace/ddgatve-screenscrapers/src/main/r")

if (!"plyr" %in% installed.packages()) install.packages("plyr")
if (!"plotrix" %in% installed.packages()) install.packages("plotrix")

df <- read.csv("../resources/data-candidates/candidates-all.csv", 
               fileEncoding="UTF-8", strip.white=TRUE)

parties <- read.csv("../resources/data-parties/parties.csv", 
                    na.strings="N/A", fileEncoding="UTF-8", strip.white=TRUE)


library(plyr)
library(plotrix)

iqr <- c()
iks <- c()
for (saeima in c(11)) {
  LParties <- subset(parties, Saeima == saeima & Procenti >= 5.0 )  
  for (myNumber in 1:length(LParties$Nr)) {
    for (distrNum in 1:5) {
      myDF <- subset(df, Saeima == saeima & 
                       PartyNum == LParties$Nr[myNumber] & 
                       DistrNum == distrNum)
      abc <- fivenum(myDF$Points)
      minElected <- myDF$Points[myDF$Result == "ievēlēts" | myDF$Result == "ievēlēta"]
      iqr[(myNumber -1)*5 + distrNum] <- abc[4]-abc[2]
      iks[(myNumber -1)*5 + distrNum] <- minElected - abc[3]
      print(sprintf("Saeima %i, saraksts %i.%s, apg. %i BIJA %i",
                    saeima, myNumber,LParties$Saisinajums[myNumber],distrNum,
                    length(minElected)))
      
    }
  }
}
  
