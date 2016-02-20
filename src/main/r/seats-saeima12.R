##################################################
### This should output Seat diagram in Saeima
##################################################

# Set this path to the one that you are using
setwd("/Users/kapsitis/workspace/")

libraries <- c("cairoDevice", "Cairo")

for (lib in libraries) {
  if (!lib %in% installed.packages()) install.packages(lib)
}

library(cairoDevice)
library(Cairo)
library(grid)
#library(gridSVG)



# R-32,V-26,L-15,K-13,Z-14
sum(32,26,15,13,14)
vien <- c(7,7,3,3,3)
na <-   c(5,6,1,2,3)
lra <-  c(3,2,1,1,1)
sc <-  c(12,3,6,1,2)
zzs <-  c(3,6,3,5,4)
nsl <-  c(2,2,1,1,1)

scColor <- "#FF3333"
nslColor <- "#A020F0"
lraColor <- "#FF6600"
zzsColor <- "#009535"
naColor <- "#DAA520"
vienotibaColor <- "#7fff00"
# zrpColor <- "#00BADF"

cols <- c(scColor,nslColor,lraColor,zzsColor,naColor,vienotibaColor)

vietas <- matrix(c(sc,nsl,lra,zzs,na,vien),nrow=5,byrow=FALSE)

theRows <- list(
  c(1,1,1,1,1,1,2,3,3,4,5,5,6,6,6,6),
  c(1,1,1,1,1,1,2,3,4,4,5,5,5,6,6,6),
  c(1,1,2,3,4,4,4,5,5,5,6,6,6),
  c(1,2,3,4,4,4,5,5,5,6,6,6,6),
  c(1,1,1,1,1,1,2,3,4,4,4,5,6,6,6),
  c(1,2,3,4,4,4,4,4,5,5,6,6,6),
  c(1,1,2,3,4,4,4,4,5,5,5,6,6,6)
)




# helpful for anti-aliasing; alpha transparency; etc.
CairoPNG(filename = "saeima-seats2.png", width = 750, height = 300,
         pointsize = 12, bg = "white",  res = NA)
pushViewport(viewport(x=0, y=0, width=0.50, height=1.25, just=c("left", "bottom"),
                      name="margin"))


#########################################
#### PARAMS
#########################################
shiftUp <- 1.66
deltaX <- 1/16
deltaY <- 1/16
sizeX <- deltaX*0.86
sizeY <- deltaY*0.86
#offsetsX <- (c(0,0,1.5,1.5,0.5,1.5,1) + 5)/16
offsetsX <- rep(5/16,7)
# paceļ uz augšu
offsetsY <- rep(shiftUp*deltaY,7)+deltaY*c(0.8,0.8,0.6,0.6,0.4,0.2,0.0)
for (i in 1:length(theRows)) {
  for (j in 1:length(theRows[[i]])) {
    cc <- cols[theRows[[i]][j]]
    grid.rect(x=offsetsX[i] + j*deltaX,
              y=offsetsY[i] + (length(theRows) + 1 - i)*deltaY, 
              width=sizeX, height=sizeY,
              gp=gpar(fill=cc, col=cc))
  }
}

grid.lines(x = rep(c(deltaX, 20*deltaX,NA),times=4),
           y = (shiftUp+0.5)*deltaY + 
             deltaY*(c(5,5,NA,3,3,NA,2,2,NA,1,1,NA)+
                       c(0.7,0.7,NA,0.5,0.5,NA,0.3,0.3,NA,0.1,0.1,NA)),
           gp=gpar(col="gray"))
districtNames <- c("R\u012Bga", "Vidzeme", 
                   "Latgale", "Kurzeme", "Zemgale")
nameShifts <- c(7.7,5.5,3.3,2.1,1)
for (kk in 1:5) {
  grid.text(districtNames[kk],
            x = deltaX, 
            y = (shiftUp+nameShifts[kk]-0.1)*deltaY, 
            just = c("left", "bottom"),
            gp = gpar(fontsize = 18, fontfamily="Calibri", col="black"))
}

acronyms <- c("SC","NSL","LRA","ZZS","NA","V")
for (k in 1:6) {
  grid.roundrect(5/16+k*2.5*deltaX,deltaY,width=2*deltaX,height=1*deltaY,
                 gp=gpar(fill=cols[k], col=cols[k]))
  grid.text(acronyms[k], x = 5/16+k*2.5*deltaX, y = 0.9*deltaY, 
            just = c("center", "bottom"),
            gp = gpar(fontsize = 15, fontface="bold", fontfamily="Calibri", col="#FFFFFF"))
}

derigasZimes <- c(307435, 251220, 104920, 115121, 126400)
vietas <- c(32,26,15,13,14)
min(derigasZimes/vietas)
max(derigasZimes/vietas)
min(derigasZimes/vietas)/max(derigasZimes/vietas)
representation <- round(derigasZimes/vietas)

balsis <- c("0.78","0.72","1.00","0.79","0.77")

for (kk in 1:5) {
  grid.text(paste0(derigasZimes[kk],"/",vietas[kk]," = ",representation[kk]),
            x = 22*deltaX, 
            y = (shiftUp+nameShifts[kk]-0.1)*deltaY, 
            just = c("left", "bottom"),
            gp = gpar(fontsize = 18, fontfamily="Calibri", col="black"))
  
  grid.text(balsis[kk],
            x = 29*deltaX, 
            y = (shiftUp+nameShifts[kk]-0.1)*deltaY, 
            just = c("left", "bottom"),
            gp = gpar(fontsize = 18, fontface="bold", fontfamily="Calibri", col="darkred"))
  
}

grid.text("Der\u012Bgas z\u012Bmes/vietas",
          x = 20.3*deltaX, 
          y = 11.3*deltaY, 
          just = c("left", "bottom"),
          gp = gpar(fontsize = 18, fontfamily="Calibri", col="blue"))

grid.text("Balsu",
          x = 29*deltaX, 
          y = 11.6*deltaY, 
          just = c("left", "bottom"),
          gp = gpar(fontsize = 18, fontfamily="Calibri", col="darkred"))

grid.text("1 v\u0113l\u0113t\u0101jam",
          x = 28*deltaX, 
          y = 11*deltaY, 
          just = c("left", "bottom"),
          gp = gpar(fontsize = 18, fontfamily="Calibri", col="darkred"))


# grid.text("z\u012Bmes",
#           x = 25*deltaX, 
#           y = 10.7*deltaY, 
#           just = c("center", "bottom"),
#           gp = gpar(fontsize = 18, fontfamily="Calibri", col="blue"))
# grid.text("/vietas=p\u0101rst\u0101vniec\u012Bba",
#           x = 22*deltaX, 
#           y = 11*deltaY, 
#           just = c("left", "bottom"),
#           gp = gpar(fontsize = 18, fontfamily="Calibri", col="blue"))
#           

popViewport()
dev.off()



