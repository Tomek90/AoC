setwd('C:/Users/Tomasz/Desktop/AoC/day2')
passwords <- read.delim('input.txt', header=F)
policy <- c()
test_text <- c()
valid <- c()
invalid <- c()
valid2 <- c()
invalid2 <- c()

#colnames(passwords) <- c("full string","policy","test","min","max","letter")

for (line in passwords[,1]) {
  policy <- append(policy,substr(line,1,(unlist(gregexpr(":",line)))-1))
  test_text <- append(test_text,substr(line,unlist(gregexpr(":",line))+2,nchar(line)))
}

passwords$policy <- policy
passwords$test <- test_text

passwords$min <- sapply(passwords$policy,function(x) substr(x,1,unlist(gregexpr("-",x))-1))
passwords$max <- sapply(passwords$policy,function(x) substr(x,unlist(gregexpr("-",x))+1,unlist(gregexpr(" ",x))-1))
passwords$letter <- sapply(passwords$policy,function(x) substr(x,unlist(gregexpr(" ",x))+1,nchar(x)))

#head(passwords)

check_pass <- function(test, min, max, letter) {
  count_l <- 0
  for (j in 1:nchar(test)) {
    if (substr(test,j,j) == letter) {
      count_l <- count_l + 1
    }
  }
    if (count_l >= as.numeric(min) & count_l <= as.numeric(max)) {
      valid <<- append(valid,test)
      
    } else {
      invalid <<- append(invalid, test)
    }
}  


check_pass2 <- function(test, min, max, letter) {
  if (substr(test,min,min) == letter & substr(test,max,max) != letter) {
    valid2 <<- append(valid2,test)
  } else if (substr(test,max,max) == letter & substr(test,min,min) != letter) {
    valid2 <<- append(valid2,test)
  } else {
    invalid2 <<- append(invalid2, test)
  }
}  

for (i in 1: nrow(passwords)) {
  check_pass(passwords[i,"test"], passwords[i,"min"], passwords[i,"max"], passwords[i,"letter"] )
  check_pass2(passwords[i,"test"], passwords[i,"min"], passwords[i,"max"], passwords[i,"letter"] )
}

# invalid <- NULL
# valid <- NULL
# invalid2 <- NULL
# valid2 <- NULL


length(invalid)
length(valid)
length(invalid2)
length(valid2)


