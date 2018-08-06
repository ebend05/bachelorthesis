setwd("/Users/elisa/Documents/Semester8/BA/Daten/CSV_Praxis")

Time <- read.csv(file= "20180713_Reisezeitmatrix_gesamt.csv", 
                 header=TRUE, sep=",", check.names = FALSE) 



aerzte <- read.csv(file= "AnzalAerzte.csv", 
                   header=TRUE, sep=";", check.names = FALSE) 

praxis <- c("B005", "B019", "B021", "B024", "B037", "B050", "B056", "B063", "B078", "B079", "B080", "B084", "B095", "B097", "B109", "B131", "B432")


pReal <- array(0, dim = c(nrow(Time), c(ncol(Time))), dimnames = list(c(rownames(Time)),c(colnames(Time[,1:516]))))  
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

Time <- Time[,1:516]
