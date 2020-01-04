addr <- matrix(data = c(3,1, 3,2, 1,3, 1,3), nrow = 4, ncol = 2)

p_htime <- c(0.8, 1.6, 2.4, 3.2)
p_time <- c(1.8, 2.4, 1.2, 0.6)
o <- 1;c <- 0;d <- 1;k <- 1;e <- 0
temp <- p_htime[1]
index <- rep(0, times=n)
s_addr <- matrix(data = 0, nrow = 4, ncol = 2)

for(d in 1:4){
  e <- 0
  for(i in 1:4){
    c <- 0
    for(j in 1:o){
      if(index[j] == i){
        c <- 0
        break;
      }else{
        c <- 1
      }
    }
    
    if(c==1){
      if(temp > p_time[i]){
        s_addr[d,1] <- addr[i,1]
        s_addr[d,2] <- addr[i,2]
        index[o] <- i
        o <- o+1
        e <- 1
      }
    }
  }
  if(e == 0){
    s_addr[d,1] <- addr[k,1]
    s_addr[d,2] <- addr[k,2]
    index[o] <- k
    o <- o+1
    k <- k+1
    temp <- p_htime[k]
  }
  print(temp)
}
