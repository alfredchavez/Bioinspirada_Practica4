library(ggplot2)
library(reshape2)

load_data <- function(filename){
  df <- read.table(filename, sep=" ", header = TRUE)
  return(df)
}

count_alpha <- function(alphanames){
  return(setNames(as.data.frame(table(alphanames)), c("alpha", "n")))
}

update_name <- function(cname, tabledata){
  for(idx in 1:length(tabledata$alpha)){
    if(tabledata$alpha[idx] == cname){
      new_name <- paste(cname, as.character(tabledata$n[idx]), sep = " -> ")
      return(new_name)
    }
  }
  return(paste(cname, "0", sep = "->"))
}

gen_graphic <- function(df){
  dfhead <- colnames(df)
  names <- c("id","real_avg",dfhead[grepl("alpha",dfhead)])
  dfavg <- subset(df, select = names)
  df_data <- subset(df, select = c("id", "closest_id", "closest_difference"))
  columns <- colnames(dfavg)[(df_data$closest_id) + 2]
  data <- c(df_data$id)
  dfcount <- count_alpha(columns)
  data <- unlist(lapply(data, function(x) return(unlist(dfavg[columns[x]])[[x]])))
  dat <- vector("list",3)
  dat[[1]] <- df_data$id
  dat[[2]] <- data
  dat[[3]] <- round(df_data$closest_difference, 2)
  df_points <- data.frame(matrix(unlist(dat), ncol = length(dat), byrow=F))
  colnames(df_points) <- c("iteration","average","label")
  newheader <- c("iteration", "real_avg", unlist(lapply(colnames(dfavg)[3:length(names)], function(x,y) return(update_name(x,y)), dfcount)))
  colnames(dfavg) <- newheader
  columns <- unlist(lapply(columns, function(x,y) return(update_name(x,y)), dfcount))
  extended_df <- melt(dfavg, id.vars="iteration",value.name = "average")
  ggplot(extended_df, aes(x=iteration, y=average, color=variable)) +
    theme_bw() + geom_line()+
    geom_point(data=df_points, aes(x=iteration, y=average, colour=columns))+
    geom_text(data=df_points, aes(x=iteration, y=average + 0.5, label=label),color="blue",size=3)
  pdf(NULL)
  ggsave("result.png", scale = 5, limitsize = FALSE)
  print("Plot saved in result.png")
}

df <- load_data("data.txt")
gen_graphic(df)