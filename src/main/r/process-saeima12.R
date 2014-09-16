# "PreNum"           "CandidateName"    "CandidateSurname"
# "Sex"              "Saeima"           "PartyNum"         "PartyAbbr"       
# "DistrNum"         "DistrName"

setwd("/home/kalvis/workspace/ddgatve-screenscrapers/src/main/r")
if (!"plyr" %in% installed.packages()) install.packages("plyr")

distrNames <- c("Rīga", "Vidzeme", "Latgale", "Kurzeme", "Zemgale")

#"Candidate","Rīga","Vidzeme","Latgale","Kurzeme","Zemgale","PartyNum","PartyAbbr"
makeLine <- function(c,r,v,l,k,z,partyNum,partyAbbr) {
#   print("partyNum")
#   print(partyNum)
#   print(c)
  firstName <- sub("(^.*) ([^ ]+)", "\\1",c)
  lastName <- sub("(^.*) ([^ ]+)", "\\2",c)
  if (!is.na(r)) {
    preNum <- r
    distrNum <- 1
  }
  else if (!is.na(v)) {
    preNum <- v
    distrNum <- 2
  }
  else if (!is.na(l)) {
    preNum <- l
    distrNum <- 3
  }
  else if (!is.na(k)) {
    preNum <- k
    distrNum <- 4
  }
  else if (!is.na(z)) {
    preNum <- z
    distrNum <- 5
  }
  else {
    print(paste0("WARNING! Candidate ", c, " without district!"))
  }
  distrName <- distrNames[distrNum]
  if (firstName == "" | lastName == "") {
    sex <- "Other"
    print(paste0("WARNING! Candidate ", c, " improperly split!"))
  }
  else if (length(grep("[sš]$", firstName)) >0 & length(grep("[sš]$", lastName)) > 0) {
    sex <- "Male"
  }
  else if (length(grep("[ae]$", firstName)) > 0 & length(grep("[ae]$", lastName)) > 0) {
    sex <- "Female"
  }
  else if (firstName == "Nelli" | firstName == "Daina" | 
             firstName == "Natalija" | firstName == "Jūlija" | 
             firstName =="Ilona" | firstName == "Nazira" |
             firstName == "Irīna" | firstName == "Iveta" |
             firstName == "Svetlana Anna" | firstName == "Astrīda" |
             firstName == "Rasma" | firstName == "Dace") {
    sex <- "Female"
  }
  else {
    sex <- "Male"
  }
  return(data.frame(PreNum = as.numeric(preNum), 
                    CandidateName = firstName, 
                    CandidateSurname = lastName,
                    Sex = sex,
                    Saeima = 12, 
                    PartyNum = as.numeric(partyNum), 
                    PartyAbbr = partyAbbr,
                    DistrNum = as.numeric(distrNum),
                    DistrName = distrName
                    ))
}
  


df <- read.csv("../resources/data-candidates/saeima12.csv",
fileEncoding="UTF-8", strip.white=TRUE)

result <- data.frame(PreNum = 0, 
                     CandidateName = "*", 
                     CandidateSurname = "*",
                     Sex = "*",
                     Saeima = 12, 
                     PartyNum = 0, 
                     PartyAbbr = "*",
                     DistrNum = 0,
                     DistrName = "*")

#"Candidate","Rīga","Vidzeme","Latgale","Kurzeme","Zemgale","PartyNum","PartyAbbr"
for (i in 1:nrow(df)) {
  theLine <- makeLine(as.character(df[i,"Candidate"]), 
                      as.integer(df[i,"Rīga"]), 
                      as.integer(df[i,"Vidzeme"]),
                      as.integer(df[i,"Latgale"]), 
                      as.integer(df[i,"Kurzeme"]), 
                      as.integer(df[i, "Zemgale"]),
                      as.integer(df[i,"PartyNum"]), 
                      as.character(df[i,"PartyAbbr"]))
  result <- rbind(result,theLine)
}

endResult <- result[ order(result$PartyNum, result$DistrNum, result$PreNum), ]

write.table(endResult, file = "mySaeima.csv", sep = ",", col.names = TRUE,
            qmethod = "double",row.names=FALSE)

