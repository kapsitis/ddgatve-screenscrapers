setwd("/home/kalvis/workspace/ddgatve-screenscrapers/src/main/r")

if (!"plyr" %in% installed.packages()) install.packages("plyr")
candidateFiles = c("candidates06.csv", "candidates07.csv", "candidates08.csv",
                   "candidates09.csv", "candidates10.csv", "candidates11.csv")

df <- read.csv(paste0("../resources/data-candidates/","candidates-all.csv"))

# for (file in candidateFiles) {
#   if (file != "candidates06.csv") {
#     df <- rbind(df, read.csv(paste0("../resources/data-candidates/",file)))
#   }
# }
# write.csv(file="../resources/data-candidates/candidates-all.csv", x=df)

df[,"Saeima"]

library(plyr)
vienotiba10 = subset(df, Saeima == "11" & PartyNum == "1" & DistrNum == "5")

points <- vienotiba10[,"Points"]
ballots <- vienotiba10[,"BallotsForParty"] 
plot((points-ballots)/ballots)

