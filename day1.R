setwd("C:/Users/Tomasz/Desktop/AoC/day1")
num_sum <- read.delim("input.txt", header = F)
colnames(num_sum) <- 'numbers'

for (ns in num_sum[,1]) {
 for (ns2 in num_sum[(match(ns,num_sum[,1])+1):nrow(num_sum),1]) {
    if ((!is.na(c(ns,ns2)) & (ns + ns2 == 2020))) {
     ans <- c(ns, ns2)
    } 
  }
 }

ans[1] * ans[2]

for (ns in num_sum[,1]) {
  for (ns2 in num_sum[,1]) {
    for (ns3 in num_sum[,1]) {
      if ((!is.na(c(ns,ns2,ns3)) & (ns + ns2 + ns3 == 2020))) {
        ans2 <- c(ns, ns2,ns3)
      } 
    }
    } 
  }

ans2[1] * ans2[2] * ans2[3]