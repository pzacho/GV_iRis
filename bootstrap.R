## setup environment
iris <- NULL
iris_raw <- NULL
compNames <- NULL
iris_colNames <- NULL

## load data
bootstrap <- function() {
  ## iris data
  iris_tmp <- read.csv("data/iRIS_ALL.csv", header=T)
  iris_raw <<- iris_tmp
  iris_colNames <<- names(iris_tmp)
  ## integer columns
  intCols <- union(
                union(iris_colNames[grep("OS",iris_colNames)],
                      iris_colNames[grep("EUR",iris_colNames)]),
                union(iris_colNames[grep("franchisees",iris_colNames)],
                      iris_colNames[grep("conversion",iris_colNames)])
  )
  iris_tmp[,intCols] <- sapply(iris_tmp[,intCols], function(x) as.numeric(gsub(",","",as.character(x))))
  iris <<- iris_tmp
  ##
  ## scrambled company names
  compNames <<- sample(read.csv("data/dummyNames.csv", header=T))
  compNames[,2] <<- apply(compNames, 1, function(x) RandomStr())
}

## scramble the company names and numbers
scramble <- function(names = NULL, pct = 1) {
  ## setup chain name scrambling
  iris_chainNames <- as.vector(unique(iris$Chain))
  ## chain scramble vector
  iris_match <- match(iris$Chain, iris_chainNames)
  ## scramble chain names
  iris$Chain <<- compNames$Company.Names[iris_match]
  ## scramble all numeric columns
  numCols <- sapply(iris,class)
  numCols <- names(numCols[numCols=="numeric"])
  iris[,numCols] <<- iris[,numCols]*pct
}

## create company code
RandomStr <- function(n=1, length=3)
{
  randomString <- c(1:n)                  # initialize vector
  for (i in 1:n)
  {
    randomString[i] <- paste(sample(c(LETTERS),
                                    length, replace=TRUE),
                             collapse="")
  }
  return(randomString)
}
