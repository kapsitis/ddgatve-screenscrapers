setwd("/home/kalvis/workspace/ddgatve-screenscrapers/src/main/r")

candidateFiles = c("candidates06.csv", "candidates07.csv", "candidates08.csv",
                   "candidates09.csv", "candidates10.csv", "candidates11.csv")

df <- read.csv(paste0("../resources/data-candidates/","candidates06.csv"))

for (file in candidateFiles) {
  if (file != "candidates06.csv") {
    df <- rbind(df, read.csv(paste0("../resources/data-candidates/",file)))
  }
}

df[,"Saeima"]

# levels(df[,"CandidateName"])

