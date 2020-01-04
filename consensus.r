getwd()
setwd("C:\\Users\\NANDU\\Desktop\\hashgraph\\Improved Timestamp")

gossip <- read.delim("gossip.txt")
cons <- read.delim("cons.txt")

names(gossip)[1] <- "gossip"
names(cons)[1] <- "cons"

#for order
library("stringr")
library("stringi")
x <-toString(cons$cons[1])
x <- str_replace_all(x, "[[:punct:]]", "")
n <- stri_stats_latex(x)[1]
n <- strtoi(n, base = 0L)
n <- n-1
y <- unlist(strsplit(toString(x), " "))
order <- rep(0, times=n)
for(i in 1:n){
  order[i] <- strtoi(y[i], base = 0L)
}

#for source and destination
a <- unlist(strsplit(toString(gossip$gossip[1]), ","))
m <- length(a)
addr <- matrix(data = 0, nrow = n, ncol = m)

for(i in 1:n){
  real <- unlist(strsplit(toString(gossip$gossip[i]), ","))
  for(j in 1:m){
    #print(real[j])
    addr[i,j] = real[j];
  }
}

src <- rep(0,n)
dest <- rep(0,n)
h_time <- rep(0,n)
for(i in 1:n){
  addr[i,1] <- strtoi(addr[i,1], base = 0L)
  addr[i,2] <- strtoi(addr[i,2], base = 0L)
  src[i] <- strtoi(addr[i,1], base = 0L)
  dest[i] <- strtoi(addr[i,2], base = 0L)
  h_time[i] <- order[i]
}

final_data <- data.frame(src, dest, h_time)
names(final_data)[1] <- "src"
names(final_data)[2] <- "dest"
names(final_data)[3] <- "h_time"
# write.table(final_data, file = "final_data.txt", sep = "\t",
#             row.names = TRUE, col.names = NA)

#Hashgraph Timestamp
# write.table(src, file = "src.txt", sep = "\t",
#             row.names = TRUE, col.names = NA)
# 
# write.table(dest, file = "dest.txt", sep = "\t",
#             row.names = TRUE, col.names = NA)

#Improving Hashgraph Timestamp
time <- sample(1:10,n,replace=T)
p_htime <- rep(1, times=n)
p_time <- rep(1, times=n)
w1 <- 0.4
w2 <- 0.6

for(i in 1:n){
  p_htime[i] <- w1*(h_time[i])
  p_time[i] <- w2*(time[i])
}
final_data <- data.frame(final_data, time, p_htime, p_time)

o <- 1;c <- 0;d <- 1;k <- 1;e <- 0
temp <- p_htime[1]
index <- rep(0, times=n)
s_addr <- matrix(data = 0, nrow = n, ncol = m)

for(d in 1:n){
  e <- 0
  for(i in 1:n){
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
  #print(temp)
}

src1 <- rep(0,n)
dest1 <- rep(0,n)
for(i in 1:n){
  s_addr[i,1] <- strtoi(s_addr[i,1], base = 0L)
  s_addr[i,2] <- strtoi(s_addr[i,2], base = 0L)
  src1[i] <- strtoi(s_addr[i,1], base = 0L)
  dest1[i] <- strtoi(s_addr[i,2], base = 0L)
}
#Improved Hashgraph Timestamp
write.table(src1, file = "src1.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

write.table(dest1, file = "dest1.txt", sep = "\t",
            row.names = TRUE, col.names = NA)
