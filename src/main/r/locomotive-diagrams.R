#setwd("/home/st/ddgatve-screenscrapers/src/main/r")
setwd("/home/kalvis/workspace/ddgatve-screenscrapers/src/main/r")

if (!"plyr" %in% installed.packages()) install.packages("plyr")
candidateFiles = c("candidates06.csv", "candidates07.csv", "candidates08.csv",
                   "candidates09.csv", "candidates10.csv", "candidates11.csv")

df <- read.csv(paste0("../resources/data-candidates/",
                      "candidates-all.csv"), fileEncoding="UTF-8")

# for (file in candidateFiles) {
#   if (file != "candidates06.csv") {
#     df <- rbind(df, read.csv(paste0("../resources/data-candidates/",file)))
#   }
# }
# write.csv(file="../resources/data-candidates/candidates-all.csv", x=df)

# df[,"Saeima"]

library(plyr)
mySaraksts = subset(df, Saeima == "11" & PartyNum == "1")

points <- mySaraksts[,"Points"]
ballots <- mySaraksts[,"BallotsForParty"] 
relativeX <- (points-ballots)/ballots

distrNum <- mySaraksts[,"DistrNum"]
elected <- mySaraksts[,"Result"]

xLower <- -0.35
xUpper <- 0.35

xx <- c(xLower,xUpper)
yy <- c(1,5)
# novadi <- c("R", "V", "L", "K", "Z")
novadi <- c("Z","K","L","V","R")

plot(xx,yy, type="n", axes = FALSE,
     xlab="",ylab="")
title(mySaraksts[1,"PartyAbbr"], sub = "sub title")
axis(1, seq(xLower,xUpper,by=0.05), seq(xLower,xUpper,by=0.05))
axis(2, 1:5, novadi)

# Compute for each district separately:
for (i in 1:5) {
  dxx <- relativeX[which(distrNum == i)]
  dyy <- rep(6-i,length(which(distrNum == i)))
  dElected <- 
  median <- fivenum(dxx)[3]
  points(dxx - median,dyy)
}

grid()
mySaraksts[,"PartyAbbr"]
