# this file contains some additional requests, calculations and results for each doctor's office

#_____________________________________________________________________________________________________________
  
  # request which rows of the time matrix contain the patients of the doctor's office
  which(Timetime[,517] == "B024")
  # save these as a new matrix, named 'time' + officeID
  timeB024 <- Timetime[1:606,1:516]
  # calculate the distance dependencies for the patients of the doctor's office and save as 'B' + officeID
  B024 = sapply(beta, function(x) {g(pReal[1:606,], timeB024, x, aerzte[,2])})
  # get the minimum values of each row/patient of the doctor's office
  rTB024 <- c()
  for (j in 1:nrow(timeB024)){      # extract all columns of the ith row
    rTB024[j]= timeB024[j,"B024"]   # find the minimum value in the same row
  }
  rTB024
  # for how many patients of the docotor's office is the minimum value <= DEW
  which(rTB024 <= mean(DEWgesamt))  
  # 606
  
# repeat for each doctor's office:

  which(Timetime[,517] == "B063")
  timeB063 <- Timetime[607:1056,1:516]
  B063 = sapply(beta, function(x) {g(pReal[607:1056,], timeB063, x, aerzte[,2])})
  rTB063 <- c()
  for (j in 1:nrow(timeB063)){      
    rTB063[j]= timeB063[j,"B063"] 
  }
  rTB063
  which(rTB063 <= mean(DEWgesamt))
  # 450
  

  which(Timetime[,517] == "B037")
  timeB037 <- Timetime[1057:2152,1:516]
  B037 = sapply(beta, function(x) {g(pReal[1057:2152,], timeB037, x, aerzte[,2])})
  rTB037 <- c()
  for (j in 1:nrow(timeB037)){      
    rTB037[j]= timeB037[j,"B037"] 
  }
  rTB037
  which(rTB037 <= mean(DEWgesamt))
  # 670
  

  which(Timetime[,517] == "B050")
  timeB050 <- Timetime[2153:6380,1:516]
  B050 = sapply(beta, function(x) {g(pReal[2153:6380,], timeB050, x, aerzte[,2])})
  rTB050 <- c()
  for (j in 1:nrow(timeB050)){      # extract all columns of the ith row  
    rTB050[j]= timeB050[j,"B050"] # find the minimum value in the same row
  }
  rTB050 
  which(rTB050 <= mean(DEWgesamt))
  # 1613
  

  which(Timetime[,517] == "B019")
  timeB019 <- Timetime[6381:6519,1:516]
  B019 = sapply(beta, function(x) {g(pReal[6381:6519,], timeB019, x, aerzte[,2])})
  rTB019 <- c()
  for (j in 1:nrow(timeB019)){       
    rTB019[j]= timeB019[j,"B019"] 
  }
  rTB019 
  which(rTB019 <= mean(DEWgesamt))
  # 73
  

  which(Timetime[,517] == "B056")
  timeB056 <- Timetime[6520:6599,1:516]
  B056 = sapply(beta, function(x) {g(pReal[6520:6599,], timeB056, x, aerzte[,2])})
  rTB056 <- c()
  for (j in 1:nrow(timeB056)){        
    rTB056[j]= timeB056[j,"B056"] 
  }
  rTB056 
  which(rTB056 <= mean(DEWgesamt))
  # 65
  

  which(Timetime[,517] == "B078")
  timeB078 <- Timetime[6600:6811,1:516]
  B078 = sapply(beta, function(x) {g(pReal[6600:6811,], timeB078, x, aerzte[,2])})
  rTB078 <- c()
  for (j in 1:nrow(timeB078)){      
    rTB078[j]= timeB078[j,"B078"] 
  }
  rTB078 
  which(rTB078 <= mean(DEWgesamt))
  # 88
  

  which(Timetime[,517] == "B131")
  timeB131 <- Timetime[6812:6908,1:516]
  B131 = sapply(beta, function(x) {g(pReal[6812:6908,], timeB131, x, aerzte[,2])})
  rTB131 <- c()
  for (j in 1:nrow(timeB131)){        
    rTB131[j]= timeB131[j,"B131"] 
  }
  rTB131 
  which(rTB131 <= mean(DEWgesamt))
  # 61
  

  which(Timetime[,517] == "B079")
  timeB079 <- Timetime[6909:7662,1:516]
  B079 = sapply(beta, function(x) {g(pReal[6909:7662,], timeB079, x, aerzte[,2])})
  rTB079 <- c()
  for (j in 1:nrow(timeB079)){        
    rTB079[j]= timeB079[j,"B079"] 
  }
  rTB079 
  which(rTB079 <= mean(DEWgesamt))
  # 396
  

  which(Timetime[,517] == "B080")
  timeB080 <- Timetime[7663:8612,1:516]
  B080 = sapply(beta, function(x) {g(pReal[7663:8612,], timeB080, x, aerzte[,2])})
  rTB080 <- c()
  for (j in 1:nrow(timeB080)){      
    rTB080[j]= timeB080[j,"B080"] 
  }
  rTB080 
  which(rTB080 <= mean(DEWgesamt))
  # 552
  

  which(Timetime[,517] == "B084")
  timeB084 <- Timetime[8613:9142,1:516]
  B084 = sapply(beta, function(x) {g(pReal[8613:9142,], timeB084, x, aerzte[,2])})
  rTB084 <- c()
  for (j in 1:nrow(timeB084)){        
    rTB084[j]= timeB084[j,"B084"] 
  }
  rTB084 
  which(rTB084 <= mean(DEWgesamt))
  # 293
  

  which(Timetime[,517] == "B097")
  timeB097 <- Timetime[9143:9542,1:516]
  B097 = sapply(beta, function(x) {g(pReal[9143:9542,], timeB097, x, aerzte[,2])})
  rTB097 <- c()
  for (j in 1:nrow(timeB097)){        
    rTB097[j]= timeB097[j,"B097"] 
  }
  rTB097 
  which(rTB097 <= mean(DEWgesamt))
  # 269
  

  which(Timetime[,517] == "B095")
  timeB095 <- Timetime[9543:10370,1:516]
  B095 = sapply(beta, function(x) {g(pReal[9543:10370,], timeB095, x, aerzte[,2])})
  rTB095 <- c()
  for (j in 1:nrow(timeB095)){        
    rTB095[j]= timeB095[j,"B095"] 
  }
  rTB095 
  which(rTB095 <= mean(DEWgesamt))
  # 367
  

  which(Timetime[,517] == "B109")
  timeB109 <- Timetime[10371:11453,1:516]
  B109 = sapply(beta, function(x) {g(pReal[10371:11453,], timeB109, x, aerzte[,2])})
  rTB109 <- c()
  for (j in 1:nrow(timeB109)){        
    rTB109[j]= timeB109[j,"B109"] 
  }
  rTB109 
  which(rTB109 <= mean(DEWgesamt))
  # 602
  

  which(Timetime[,517] == "B005")
  timeB005 <- Timetime[11454:12061,1:516]
  B005 = sapply(beta, function(x) {g(pReal[11454:12061,], timeB005, x, aerzte[,2])})
  rTB005 <- c()
  for (j in 1:nrow(timeB005)){       
    rTB005[j]= timeB005[j,"B005"] 
  }
  rTB005 
  which(rTB005 <= mean(DEWgesamt))
  # 316
  

  which(Timetime[,517] == "B021")
  timeB021 <- Timetime[12062:12453,1:516]
  B021 = sapply(beta, function(x) {g(pReal[12062:12453,], timeB021, x, aerzte[,2])})
  rTB021 <- c()
  for (j in 1:nrow(timeB021)){        
    rTB021[j]= timeB021[j,"B021"] 
  }
  rTB021 
  which(rTB021 <= mean(DEWgesamt))
  # 216
  

  which(Timetime[,517] == "B432")
  timeB432 <- Timetime[12454:15388,1:516]
  B432 = sapply(beta, function(x) {g(pReal[12454:15388,], timeB432, x, aerzte[,2])})
  rTB432 <- c()
  for (j in 1:nrow(timeB432)){        
    rTB432[j]= timeB432[j,"B432"] 
  }
  rTB432 
  which(rTB432 <= mean(DEWgesamt))
  # 991

#_____________________________________________________________________________________________________________

# request to find the optimal (minimal) distance dependency (beta) for each doctor's office
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
  
#_____________________________________________________________________________________________________________

# requests to get the amount of patients of each doctor's office
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
