ppReal <- array(0, dim = c(nrow(Time), c(ncol(Time))), dimnames = list(c(rownames(Time)),c(colnames(Time[,1:516]))))  
# fills Matrix with real probability of the patients doctors 
# if the patient went to a doctor the probability is 100% = 1, the others get 0
for (i in 1: nrow(Time[,1:516])) {
  for (j in 1:length(praxis)) {
    if (Time[i,"Bezeichner"] == praxis[j]){
      pReal[i,j] <- 1
    }else{
      pReal[i,j] <- 0
    }
  }
}

#_____________________________________________________________________________________________________________

pReal[1,1]
colnames(Time)
colnames(pReal)
w = colnames(Time) %in% colnames(pReal)
w
pTime = Time[,w]
dim(pTime)
all(colnames(Time) == colnames(pReal))

#_____________________________________________________________________________________________________________

pij = function(time, beta, aj) {
  num = aj^1 * exp(-time * beta)#
  rowsum = apply(num, 1, sum)
  denom = matrix(rowsum, nrow(num), ncol(num))
  num / denom
}

# ohne Aj
# pij = function(time, beta) {
#   num = exp(-time * beta)
#   rowsum = apply(num, 1, sum)
#   denom = matrix(rowsum, nrow(num), ncol(num))
#   num / denom
# }

g = function(o, p, beta, aj) { 
  sum(abs(o - pij(p, beta, aj)))
}

# ohne Aj
# g = function(o, p, beta) { 
#   sum(abs(o - pij(p, beta)))
# }

beta = seq(0, 2, .01 )


ans = sapply(beta, function(x) {g(pReal, pTime, x, aerzte[,2])})

# ohne Aj
#ans = sapply(beta, function(x) {print(x); g(pReal, pTime, x)})

timeAlles <- ans

#_____________________________________________________________________________________________________________

setwd("/Users/elisa/Documents/Semester8/BA/Daten/CSV_Praxis")
Timetime <- read.csv(file="20180713_Reisezeitmatrix_gesamt.csv", header=TRUE, sep=",", check.names = FALSE) 


which(Timetime[,517] == "B024")
# vib 2153 bis 3228
timeB024 <- Timetime[1:606,1:516]
B024 = sapply(beta, function(x) {g(pReal[1:606,], timeB024, x, aerzte[,2])})

which(Timetime[,517] == "B063")
# vib 2153 bis 3228
timeB063 <- Timetime[607:1056,1:516]
B063 = sapply(beta, function(x) {g(pReal[607:1056,], timeB063, x, aerzte[,2])})

which(Timetime[,517] == "B037")
# vib 2153 bis 3228
timeB037 <- Timetime[1057:2152,1:516]
B037 = sapply(beta, function(x) {g(pReal[1057:2152,], timeB037, x, aerzte[,2])})

which(Timetime[,517] == "B050")
# vib 2153 bis 3228
timeB050 <- Timetime[2153:6380,1:516]
B050 = sapply(beta, function(x) {g(pReal[2153:6380,], timeB050, x, aerzte[,2])})

which(Timetime[,517] == "B019")
# vib 2153 bis 3228
timeB019 <- Timetime[6381:6519,1:516]
B019 = sapply(beta, function(x) {g(pReal[6381:6519,], timeB019, x, aerzte[,2])})

which(Timetime[,517] == "B056")
# vib 2153 bis 3228
timeB056 <- Timetime[6520:6599,1:516]
B056 = sapply(beta, function(x) {g(pReal[6520:6599,], timeB056, x, aerzte[,2])})

which(Timetime[,517] == "B078")
# vib 2153 bis 3228
timeB078 <- Timetime[6600:6811,1:516]
B078 = sapply(beta, function(x) {g(pReal[6600:6811,], timeB078, x, aerzte[,2])})

which(Timetime[,517] == "B131")
# vib 2153 bis 3228
timeB131 <- Timetime[6812:6908,1:516]
B131 = sapply(beta, function(x) {g(pReal[6812:6908,], timeB131, x, aerzte[,2])})

which(Timetime[,517] == "B079")
# vib 2153 bis 3228
timeB079 <- Timetime[6909:7662,1:516]
B079 = sapply(beta, function(x) {g(pReal[6909:7662,], timeB079, x, aerzte[,2])})

which(Timetime[,517] == "B080")
# vib 2153 bis 3228
timeB080 <- Timetime[7663:8612,1:516]
B080 = sapply(beta, function(x) {g(pReal[7663:8612,], timeB080, x, aerzte[,2])})

which(Timetime[,517] == "B084")
# vib 2153 bis 3228
timeB084 <- Timetime[8613:9142,1:516]
B084 = sapply(beta, function(x) {g(pReal[8613:9142,], timeB084, x, aerzte[,2])})

which(Timetime[,517] == "B097")
# vib 2153 bis 3228
timeB097 <- Timetime[9143:9542,1:516]
B097 = sapply(beta, function(x) {g(pReal[9143:9542,], timeB097, x, aerzte[,2])})

which(Timetime[,517] == "B095")
# vib 2153 bis 3228
timeB095 <- Timetime[9543:10370,1:516]
B095 = sapply(beta, function(x) {g(pReal[9543:10370,], timeB095, x, aerzte[,2])})

which(Timetime[,517] == "B109")
# vib 2153 bis 3228
timeB109 <- Timetime[10371:11453,1:516]
B109 = sapply(beta, function(x) {g(pReal[10371:11453,], timeB109, x, aerzte[,2])})

which(Timetime[,517] == "B005")
# vib 2153 bis 3228
timeB005 <- Timetime[11454:12061,1:516]
B005 = sapply(beta, function(x) {g(pReal[11454:12061,], timeB005, x, aerzte[,2])})

which(Timetime[,517] == "B021")
# vib 2153 bis 3228
timeB021 <- Timetime[12062:12453,1:516]
B021 = sapply(beta, function(x) {g(pReal[12062:12453,], timeB021, x, aerzte[,2])})

which(Timetime[,517] == "B432")
# vib 2153 bis 3228
timeB432 <- Timetime[12454:15388,1:516]
B432 = sapply(beta, function(x) {g(pReal[12454:15388,], timeB432, x, aerzte[,2])})

#_____________________________________________________________________________________________________________

# which(timeAlles == min(timeAlles))
# [1] 5
# > timeAlles[39]
# [1] 30364.02
# > beta[39]
# [1] 0.4

minimum <- function(timeee){
  x <- (timeee == min(timeee))
  return(beta[x])
}





minimum(B005)
# 0.12
minimum(B019)
# 0.1
minimum(B021)
# 0.06
minimum(B024)
# 0.13
minimum(B037)
# 0.21
minimum(B050)
# 0.5
minimum(B056)
# 0.1
minimum(B063)
# 0.08
minimum(B078)
# 0
minimum(B079)
# 0.12
minimum(B080)
# 0.05
minimum(B084)
# 0.03
minimum(B095)
# 0
minimum(B097)
# 0.02
minimum(B109)
# 0.09
minimum(B131)
# 0
minimum(B432)
# 0.03

 dim(timeB005)
# 608 516
 dim(timeB019)
# 139 516
 dim(timeB021)
# 392 516
 dim(timeB024)
# 606 516
 dim(timeB037)
# 1096  516
 dim(timeB050)
# 4228  516
 dim(timeB056)
#  80 516
 dim(timeB063)
# 450 516
 dim(timeB078)
# 212 516
 dim(timeB079)
# 754 516
 dim(timeB080)
# 950 516
 dim(timeB084)
# 530 516
 dim(timeB095)
# 828 516
 dim(timeB097)
# 400 516
 dim(timeB109)
# 1083  516
 dim(timeB131)
#  97 516
 dim(timeB432)
# 2935  516