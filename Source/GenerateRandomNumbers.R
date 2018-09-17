generaterandom <- function(n, inf, sup){
  l <- sample(inf:sup, n, TRUE)
  return(l)
}

writetofile <- function(numberlist, filename){
  write(numberlist, filename, ncolumns = 1)
}


numbers <- generaterandom(500, 1, 100)
print("RANDOM NUMBERS:")
print(numbers)
print("random numbers saved in randomnumbers.txt")
writetofile(numbers, "randomnumbers.txt")