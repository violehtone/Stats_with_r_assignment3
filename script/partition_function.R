## @knitr partitionFunction
# INPUT is the required number of partitions k and a single factor or a
# list of factors f to stratify over, each of the same length as the 
# number of rows in the data to be stratified (see the help for the 
# tapply() function).
#
# OUTPUT is an integer vector of the same length as the number of rows in the 
# data, containing partition labels (1 .. k). This vector allocates each row in 
# the data structure to a partition, and can be used to select rows from a
# certain partition.
# 
# # Example for the iris data set, 5 partitions:
# plabel <- partition(k=5, f=iris$Species)

partition <- function(k, f) {
  if (is.atomic(f)) {
    lenf <- length(f)
  } else {
    lenf <- length(f[[1]])
  }
  part <- vector(mode="integer", length=lenf)
  tapply(X=1:length(part), INDEX=f, FUN=function(i) {
    part[i] <<- as.integer((sample(1:length(i)) + sample(1:k,1))%%k + 1)
  }
  )
  return(part)
}