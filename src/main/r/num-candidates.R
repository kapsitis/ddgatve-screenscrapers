library(gmp)
t <- as.bigz(3)

cand <- c(32,14,4,35,10,
          34,35,35,32,31,  
          35,35,33)

candKurz <- c(13, 5, 5, 16, 3, 
              14, 16, 16, 13, 6, 
              16, 16, 13)

powers <- lapply(candKurz,function(x) { pow.bigz(t,x)})

total <- as.bigz(0)
for (i in 1:13) {
  total <- add.bigz(total, powers[[i]])
}