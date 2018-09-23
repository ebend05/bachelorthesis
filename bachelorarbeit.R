
# load travel time for each patient-doctor-interaction
# setwd('/Users/elisa/Documents/Semester8/BA/Daten/CSV_Praxis')
# Time <- read.csv(file="20180713_Reisezeitmatrix_gesamt.csv", header=TRUE, sep=",", check.names = FALSE) 
# aerzte <- read.csv(file="AnzahlAerzte.csv", header=TRUE, sep=",", check.names = FALSE)
#_____________________________________________________________________________________________________________

# create new Matrix pReal filled with the real patient-doctor-interactions
# if the patient went to a doctor the probability is 100% = 1, else 0
pReal <- array(0, dim = c(nrow(Time), c(ncol(Time))), dimnames = list(c(rownames(Time)),c(colnames(Time[,1:516]))))  
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

# make sure Time and pReal have the same columns
# delete last column of Time and save new matrix as pTime
colnames(Time)
colnames(pReal)
w = colnames(Time) %in% colnames(pReal)
w
pTime = Time[,w]
dim(pTime)
all(colnames(Time) == colnames(pReal))

#_____________________________________________________________________________________________________________

# function pij to calculate the gravity model for modelling the probability of interaction  
# with alpha = 1
pij = function(time, beta, aj) {
  num = aj^1 * exp(-time * beta)
  rowsum = apply(num, 1, sum)
  denom = matrix(rowsum, nrow(num), ncol(num))
  num / denom
}

# function to calculate the difference between pReal and pTime while calling pij
g = function(o, p, beta, aj) { 
  sum(abs(o - pij(p, beta, aj)))
}

# set beta as sequence from 0 to 2 in 0.01
beta = seq(0, 2, .01 )

# call function g 
# with the amount of doctor's as aj
ans = sapply(beta, function(x) {g(pReal, pTime, x, aerzte[,2])})
Diff <- ans

# function to find the optimal beta
minimum <- function(timeee){
  x <- (timeee == min(timeee))
  return(beta[x])
}

# result
minimum(Diff)
# 0.38

# plot result
plot(x=beta, 
     y=Diff, 
     type="p", 
     main = "Iterative Kalibrierung mit Beta", 
     xlab = "Beta", 
     ylab = "Summe der |pR - pij|-Differenzen aller Patienten")
     abline(v=0.38, 
     col="red")
#_____________________________________________________________________________________________________________

# calculate the expected distance with the optimal beta 
dew <-  function(p, beta, aij) { sum(pij(p, beta, aij)*p)}

# with result beta = 0.38
DEW <-  dew(pTime[2 ,] , 0.38 , aerzte [ ,2])
DEW

#_____________________________________________________________________________________________________________

# get the patients whose real travel time is under 1h

  # shortestTime <- Timetime
  # m <- 0
  # for (m in 1:nrow(Timetime)){     
  #   index <- which(realTime > 60.000)
  # }
  # 
  # index <- which(Timetime[m,Timetime[m,"Bezeichner"]] <= 60.000)
  # 
  # shortestTime <- Timetime[-index, ]
  # 
  # realTime2 <- c()
  # for (j in 1:nrow(shortestTime)){      # extract all columns of the ith row  
  #   realTime2[j]= shortestTime[j,shortestTime[j,"Bezeichner"]] # find the minimum value in the same row
  # }
  # realTime2 



