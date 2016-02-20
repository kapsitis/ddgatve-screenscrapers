setwd("/home/st/ddgatve-screenscrapers/src/main/r")
#setwd("/home/kalvis/workspace/ddgatve-screenscrapers/src/main/r")

if (!"plyr" %in% installed.packages()) install.packages("plyr")
# candidateFiles = c("candidates06.csv", "candidates07.csv", "candidates08.csv",
#                    "candidates09.csv", "candidates10.csv", "candidates11.csv")

df <- read.csv("votes-per-district.csv",                      
               na.strings="N/A", fileEncoding="UTF-8", strip.white=TRUE)

library(plyr)

# xx <- partiesForSaeima[partiesForSaeima$Procenti >= 2.0,]


theNumbers <-c(4,7,8,11,12,13)

partiesForSaeima <-subset(df, Num %in% c(4,7,8,11,12,13) & Apgabals == "Latgale")

if (exists("SainteLagueSeq")) {
  rm(SainteLagueSeq)
}

zimes <- partiesForSaeima$Zimes


N <- 18

slFractions <- c()
slNames <- c()

for (num in 1:length(zimes)) {
  slFractions <- 
    c(slFractions,zimes[num]/seq(1, by=2, length.out=N))
  slNames <- c(slNames,rep(theNumbers[num],N))
}

myFrame <- data.frame(names = slNames, fractions = slFractions)

theOrder <- order(myFrame$fractions, 
                  decreasing=TRUE)
commonList <- myFrame[theOrder, c("names", "fractions")]

# Print first 15 people that are elected according to 
# the Sainte Lague method. 
theList <- commonList[1:N,]

for (i in 1:length(theNumbers)) {
  print(paste0(theNumbers[i]," is ",length(which(theList$names==theNumbers[i]))))
}

sc <- c(13,3,6,1,2)
vien <- c(7,7,3,3,3)
zzs <- c(3,6,3,5,4)
na <- c(5,6,1,2,3)
nsl <- c(2,2,1,1,1)
lra <- c(2,2,1,1,1)

# 7,6,2    3,6,2


