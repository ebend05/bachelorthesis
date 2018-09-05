pReal[1,1]
colnames(Time)
colnames(pReal)
w = colnames(Time) %in% colnames(pReal)
w
pTime = Time[,w]
dim(pTime)
all(colnames(Time) == colnames(pReal))

pij = function(time, beta) {
  num = exp(-time * beta)
  rowsum = apply(num, 1, sum)
  denom = matrix(rowsum, nrow(num), ncol(num))
  num / denom
}


pij = function(time, beta, aj) {
  #one = exp(-time * beta)
  #print(one)
  #aij = apply(aerzte, 1, return(aij <- aerzte$Anzahl))
  #aij = tapply(time, 1, return(abc)) #get(a) {a[apply()]}) 
    #apply(time, 1, function(a) {a[apply(aerzte, 1, return(aij <- aerzte$Anzahl))]})
              #function(a) any(a %in% (colnames(time) == aerzte$Bezeichner && return(aerzte$Anzahl))))#any(a %in% aerzte$Bezeichner)return(aerzte$Anzahl))
  #print("aij")
  #print(aij)
  #print(aij)
  num = aj^1 * exp(-time * beta)#
  #num = exp(-time * beta)#
  #print(num)
  #print("num")
  #print(num)
  rowsum = apply(num, 1, sum)
  denom = matrix(rowsum, nrow(num), ncol(num))
  num / denom
  #print(pij)
}



# check:
p = pij(pTime, 1)
max(abs(apply(p, 1, sum)- 1))

g = function(o, p, beta, aj) { 
  sum(abs(o - pij(p, beta, aj)))
}

g = function(o, p, beta) { 
  sum(abs(o - pij(p, beta)))
}


beta = seq(0, 2, .1 )

ans = sapply(beta, function(x) {print(x); g(pReal, pTime, x, aerzte[,2])})
ans = sapply(beta, function(x) {print(x); g(pReal, pTime, x)})


timeAlles <- ans

#from0to2by0.01 <- ans
#from0to1by0.01 <- ans
plot(from0to1by0.01)

plot(from0to2by0.01)


#plot0.05 <- plot(ans)


plot(from0to2by0.01,type="l",col="red") 
lines(from0to2by0.01,col="green") # die mit b beinhalten aij



x = 1:150
plot(x, exp(-x * .1), type = 'l')






setwd("/Users/elisa/Documents/Semester8/BA/Daten/CSV_Praxis")
Timetime <- read.csv(file="20180713_Reisezeitmatrix_gesamt.csv", header=TRUE, sep=",", check.names = FALSE) 
Time050 <- Timetime[,(Timetime$Bezeichner)]

# from0to2by0.01[38]
# [1] 30365.9
# beta[38]
# [1] 0.37

# Levels: B005 B019 B021 B024 B037 B050 B056 B063 B078 B079 B080 B084 B095 B097 B109 B131 B432

which(Timetime[,517] == "B024")
# vib 2153 bis 3228
timeB024 <- Timetime[1:606,]

which(Timetime[,517] == "B063")
# vib 2153 bis 3228
timeB063 <- Timetime[607:1056,]

which(Timetime[,517] == "B037")
# vib 2153 bis 3228
timeB037 <- Timetime[1057:2152,]

which(Timetime[,517] == "B050")
# vib 2153 bis 3228
timeB050 <- Timetime[2153:6380,]

which(Timetime[,517] == "B019")
# vib 2153 bis 3228
timeB019 <- Timetime[6381:6519,]

which(Timetime[,517] == "B056")
# vib 2153 bis 3228
timeB056 <- Timetime[6520:6599,]

which(Timetime[,517] == "B078")
# vib 2153 bis 3228
timeB078 <- Timetime[6600:6811,]

which(Timetime[,517] == "B131")
# vib 2153 bis 3228
timeB131 <- Timetime[6812:6908,]

which(Timetime[,517] == "B079")
# vib 2153 bis 3228
timeB079 <- Timetime[6909:7662,]

which(Timetime[,517] == "B080")
# vib 2153 bis 3228
timeB080 <- Timetime[7663:8612,]

which(Timetime[,517] == "B084")
# vib 2153 bis 3228
timeB084 <- Timetime[8613:9142,]

which(Timetime[,517] == "B097")
# vib 2153 bis 3228
timeB097 <- Timetime[9143:9542,]

which(Timetime[,517] == "B095")
# vib 2153 bis 3228
timeB095 <- Timetime[9543:10370,]

which(Timetime[,517] == "B109")
# vib 2153 bis 3228
timeB109 <- Timetime[10371:11453,]

which(Timetime[,517] == "B005")
# vib 2153 bis 3228
timeB005 <- Timetime[11454:12061,]

which(Timetime[,517] == "B021")
# vib 2153 bis 3228
timeB021 <- Timetime[12062:12453,]

which(Timetime[,517] == "B432")
# vib 2153 bis 3228
timeB432 <- Timetime[12454:15388,]






# which(timeAlles == min(timeAlles))
# [1] 5
# > timeAlles[5]
# [1] 30364.02
# > beta[5]
# [1] 0.4
# > 








