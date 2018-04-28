setwd("E:/Documents/Practice/PW")

library(dplyr)

init = 100

set.seed(2018)

## Good way for doing simulations

random_toss = runif(100,0,1)

val = sapply(random_toss, function(x){ifelse(x>=0.5,1.1,0.9)})

print(init*prod(val))

val_1 = matrix(c(init,rep(0,99)),100,1)

for(i in 1:length(val)){
  if(i==1){
    val_1[i] = init*val[i]
  } else{
    val_1[i] = val_1[i-1]*val[i]
  }
}

plot(val_1,type='l')# my first random walk
