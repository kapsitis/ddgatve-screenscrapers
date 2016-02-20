setwd("/home/st/ddgatve-screenscrapers/src/main/r")
#setwd("/home/kalvis/workspace/ddgatve-screenscrapers/src/main/r")

if (!"Cairo" %in% installed.packages()) install.packages("Cairo")
if (!"ggplot2" %in% installed.packages()) install.packages("ggplot2")
if (!"grid" %in% installed.packages()) install.packages("grid")
if (!"plotrix" %in% installed.packages()) install.packages("plotrix")
if (!"plyr" %in% installed.packages()) install.packages("plyr")

library(Cairo)
library(ggplot2)
library(grid)
library(plotrix)
library(plyr)




multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



df <- read.csv("../resources/data-candidates/candidates-all.csv", 
               fileEncoding="UTF-8", strip.white=TRUE)
parties <- read.csv("../resources/data-parties/parties.csv", 
                    na.strings="N/A", fileEncoding="UTF-8", strip.white=TRUE)

#distrs <- rep(c("R","V","L","K","Z"), times=5)


getMiddleSection <- function(rows,myMode) {
  # 2 Middle quartiles
  if (myMode == "middle") {
    idxRange <- fivenum(1:nrow(myDF))
    return(middleSection <- ceiling(idxRange[2]):floor(idxRange[4]))
  }
  # Everybody
  else if (myMode == "all") {
    return(1:rows)      
  }
  # All except first 3
  else if (myMode == "skip3") {
    middleSection <- 4:rows
    
  }
}




for (saeima in c(11)) {
  
  naColor <- "#DAA520"
  vienotibaColor <- "#7fff00"
  scColor <- "#FF0B08"
  zrpColor <- "#00BADF"
  pllColor <- "#A020F0"
  zzsColor <- "#009535"
  if (saeima == 11) {
    colorList <- c(vienotibaColor,zrpColor, scColor, naColor, zzsColor)
    abbrList <- c("Vien.","ZRP","SC","NA","ZZS")
  }
  if (saeima == 10) {   
    colorList <- c(vienotibaColor,scColor, zzsColor, pllColor, naColor)
    abbrList <- c("Vien.","SC","ZZS","PLL","NA")
  }
  
  partiesForSaeima <- subset(parties, Saeima == saeima & Procenti >= 5.0)
  theNumbers <- partiesForSaeima$Nr
  theData <- rep(0,5)
  maleMean <- rep(0,5)
  femaleMean <- rep(0,5)
  maleCount <- rep(0,5)
  femaleCount <- rep(0,5)
  commonMean <- rep(0,5)
  commonCount <- rep(0,5)
  theR <- rep(0,5)
  
  for (myMode in c("all","skip3")) {
    
    #  for (idx in 1)
    for (idx in 1:length(theNumbers)) {
      # Process all parties that got past 5% barrier
      #  for (idx in c(1)) {
      myNumber <- theNumbers[idx]
      
      newDF <- data.frame(PostNum=integer(),
                          PreNum=integer(), 
                          CandidateName=character(),
                          CandidateSurname=character(),
                          Sex=character(),
                          Saeima=integer(),
                          PartyNum=integer(),
                          PartyAbbr=character(),
                          DistrNum=integer(),
                          DistrName=character(),
                          BallotsForParty=integer(),
                          Pluses=integer(),
                          Minuses=integer(),
                          Points=integer(),
                          Result=character(),
                          RelX=numeric(),
                          stringsAsFactors=FALSE) 
      # visit all districts and combine middle two quartiles in a single big list
      for (i in 1:5) {
        myDF <- subset(df, Saeima == saeima & PartyNum == myNumber & DistrNum == i)    
        myDF$RelX <- (myDF$Points - myDF$BallotsForParty)
        myDF <- myDF[order(myDF$RelX, decreasing=TRUE),]   
        
        middleSection <- getMiddleSection(nrow(myDF),myMode)
        
        middleDF <- myDF[middleSection,]       
        newDF <- rbind(newDF,middleDF)
      } 
      newDF$IsMale <- 0 + (newDF$Sex == "Male")
      
      maleMean[idx] <- mean(newDF$RelX[newDF$Sex == "Male"])
      maleCount[idx] <- length(newDF$RelX[newDF$Sex == "Male"])
      femaleMean[idx] <-  mean(newDF$RelX[newDF$Sex == "Female"])
      femaleCount[idx] <- length(newDF$RelX[newDF$Sex == "Female"])
      commonMean[idx] <- mean(newDF$RelX)
      commonCount[idx] <- length(newDF$RelX)
      theData[idx] <- maleMean[idx] - femaleMean[idx] 
      
      
      theR[idx] <- as.numeric(cor.test(newDF$RelX,newDF$IsMale)[["estimate"]])
    }
#     CairoPNG(family="Times",width = 960, height = 480, pointsize = 16, 
#         filename=paste0(getwd(),"/men-women-points-",saeima,"-",myMode,".png"))
   
# CairoFonts(
#   regular="FreeSans:style=Medium",
#   bold="FreeSans:style=Bold",
#   italic="FreeSans:style=Oblique",
#   bolditalic="FreeSans:style=BoldOblique"
# )
par(family="Times")
png(width = 960, height = 480, pointsize = 16, 
            type = "cairo-png",
            filename=paste0(getwd(),"/men-women-points-",saeima,"-",myMode,".png"))
    
    
    partyNames <- c("Vienotība","Reformu\nPartija","Saskaņas\nCentrs",
                    "Nacionālā\nApvienība","Zaļie\nZemnieki")
    datC <- data.frame(PartyNum = rep(c(1,3,6,8,11),times=3), StackOrder = rep(5:1,times=3),
                       PartyAbbr = rep(partyNames, times=3),
                       Means = c(commonMean,femaleMean,maleMean),
                       Counts = c(commonCount,femaleCount,maleCount),
                       Sex = rep(c("Visi","Sievietes","Vīrieši"), each=5))
    
    datC$PartyAbbr <- factor(datC$PartyAbbr, levels=rev(unique(as.character(datC$PartyAbbr))) )
    datC$Sex <- factor(datC$Sex, levels=rev(unique(as.character(datC$Sex)) ))
    
    
    
    p1 <- ggplot() + geom_bar(stat="identity", data=datC, 
                              aes(x=PartyAbbr, y=Means, fill=Sex),
                              position=position_dodge()) + 
      #      geom_bar(stat="identity", position=position_dodge()) +
      scale_fill_manual("Dzimums",values=rev(c("#999999", "pink","lightblue"))) + 
      theme(text = element_text(size=20)) +
      theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank())+
      theme(axis.ticks = element_blank())+
      scale_x_discrete(name="") +
      scale_y_continuous(name="Vidējais grozījums (plusi-svītrojumi)")+
      coord_flip()
    
    datD <-  data.frame(PartyNum = c(1,3,6,8,11), Delta = theData,
                        PartyAbbr = partyNames)
    datD$PartyAbbr <- factor(datD$PartyAbbr, levels=rev(unique(as.character(datD$PartyAbbr))) )
    
    p2 <- ggplot() + geom_bar(stat="identity", data=datD, 
                              aes(x=PartyAbbr, y=Delta, fill=PartyAbbr)) + 
      #      geom_bar(stat="identity", position=position_dodge()) +
      scale_fill_manual(values=rev(colorList)) + 
      theme(text = element_text(size=20)) +
      theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank())+
      theme(axis.ticks = element_blank())+
      theme(legend.position = "none") +
      scale_x_discrete(name="") +
      scale_y_continuous(name="Vīriešu punktu pārsvars")+
      coord_flip()
    multiplot(p1, p2, cols=2)
    
    dev.off()
  }
  
}




png(file="AIDS.png", width=5, height=5, units="in", type="cairo", res=800)
xyplot(1 ~ 1,  type="b", ylab=expression(sqrt(CD4/µ*l)))
dev.off()