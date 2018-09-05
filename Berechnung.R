pReal[1,1]
colnames(Time)
colnames(pReal)
w = colnames(Timetime) %in% colnames(pReal)
w
pTime = Timetime[,w]
dim(pTime)
all(colnames(pTime) == colnames(pReal))

 pij = function(time, beta) {
   #aij = apply(aerzte, 1, aerzte[,2])
   #aij = aerzte[,2]
   num = 1^1 * exp(-time * beta)
   rowsum = apply(num, 1, sum)
   #return("aij")
   #return(aij)
   denom = matrix(rowsum, nrow(num), ncol(num))
   num / denom
   #return("pij")
   #return(pij)
 }
 
 


# pij = function(time, beta) {
#   #one = exp(-time * beta)
#   #print(one)
#   #aij = testm[1,]
#   #print(aij)
#   num = 1^1 * exp(-time * beta)
#   #print("num")
#   #print(num)
#   rowsum = apply(num, 1, sum)
#   denom = matrix(rowsum, nrow(num), ncol(num))
#   num / denom
#   #print(num)
# }




#mode(Time) = "numeric"
#Time = as.numeric(as.matrix(Time))

# check:
p = pij(pTime, 1)
max(abs(apply(p, 1, sum)- 1))


p = abc(pTime, 1)


g = function(o, p, beta) { 
  sum(abs(o - pij(p, beta)))
}

beta = seq(0, 2, .1 )

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
