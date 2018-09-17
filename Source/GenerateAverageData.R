getnumbers <- function(filename){
  randomnumbers <- read.delim(filename, header = FALSE)
  randomnumbers <- lapply(randomnumbers, as.double)
  return(randomnumbers$V1)
}

get_real_average <- function(datalist){
  datalen <- length(datalist)
  avglist <- vector("numeric", datalen)
  avgval <- 0
  for (idx in 1:datalen) {
    avgval <- (avgval * (idx - 1) + datalist[idx]) / idx
    avglist[idx] <- avgval
  }
  return(avglist)
}

get_alpha_average <- function(datalist, alpha){
  datalen <- length(datalist)
  avglist <- vector("numeric", datalen)
  avgval <- 0
  for (idx in 1:datalen) {
    avgval <- alpha * avgval + (1 - alpha) * datalist[idx]
    avglist[idx] <- avgval
  }
  return(avglist)
}

write_formated_data <- function(data, realavg, alpha_avg, alphas){
  col_names <- unlist(lapply(alphas, function(x) return(paste("alpha", as.character(x),sep = "_"))))
  col_names <- c("id", "values", "real_avg" , col_names, "closest_id", "closest_difference")
  datalen <- length(data)
  alphalen <- length(alpha_avg)
  datalist <- vector("list", alphalen + 5)
  datalist[[1]] <- 1:datalen
  datalist[[2]] <- data
  datalist[[3]] <- realavg
  for (idx in 1:alphalen) {
    datalist[[3 + idx]] = alpha_avg[[idx]]
  }
  datalist[[alphalen + 4]] = vector("numeric", datalen)
  datalist[[alphalen + 5]] = vector("numeric", datalen)
  for (idx in 1:datalen){
    difference <- 500.0
    indx <- -1
    for (idy in 1:alphalen) {
      if (abs(alpha_avg[[idy]][idx] - realavg[idx]) < difference) {
        difference <- abs(alpha_avg[[idy]][idx] - realavg[idx])
        indx <- idy
      }
    }
    datalist[[alphalen + 4]][idx] = indx
    datalist[[alphalen + 5]][idx] = difference
  }
  df <- data.frame(matrix(unlist(datalist), ncol=length(datalist), byrow=F))
  colnames(df) <- col_names
  print("CALCULATED AVERAGES: ")
  print(df)
  write.table(df, file = "data.txt", row.names = FALSE)
  print("average data saved in data.txt")
  return(df)
}

lista <- getnumbers("randomnumbers.txt")
avgs <- get_real_average(lista)
alphas <- c(0.25, 0.5, 0.75, 0.05, 0.95)
avg_alphas <- lapply(alphas, get_alpha_average, datalist = lista)
write_formated_data(lista, avgs, avg_alphas, alphas = alphas)
