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


pij = function(time, beta) {
  #one = exp(-time * beta)
  #print(one)
  aij = testm[1,]
  #print(aij)
  num = aij^1 * exp(-time * beta)
  #print("num")
  #print(num)
  rowsum = apply(num, 1, sum)
  denom = matrix(rowsum, nrow(num), ncol(num))
  num / denom
  #print(num)
}



#mode(Time) = "numeric"
#Time = as.numeric(as.matrix(Time))

# check:
p = pij(pTime, 1)
max(abs(apply(p, 1, sum)- 1))

g = function(o, p, beta) { 
  sum(abs(o - pij(p, beta)))
}

beta = seq(0, 2, .01 )

ans = sapply(beta, function(x) {print(x); g(pReal, testm, x)})

bfrom0to2by0.01 <- ans

#from0to2by0.01 <- ans
#from0to1by0.01 <- ans
plot(from0to1by0.01)

plot(from0to2by0.01)


#plot0.05 <- plot(ans)


plot(from0to2by0.01,type="l",col="red") 
lines(bfrom0to2by0.01,col="green") # die mit b beinhalten aij



x = 1:150
plot(x, exp(-x * .1), type = 'l')
