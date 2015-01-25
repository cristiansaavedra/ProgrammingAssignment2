## Programming Assignment 2: Lexical Scoping for R Programming at Coursera Data Science Specialization
## This functions do a caching of the Inverse of a Matrix 

## The makeCacheMatrix function create a structured class with its methods for 
## set a cached Inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() { 
    return(x)
  }
  setInverse <- function() {
    i <<- solve(x)
  }
  getInverse <- function() {
    return(i)
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function call the makeCacheMatrix methods for create an instance 
## of the matrix and create its inverse matrix if its Inverse is not created yet.
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("Getting cached Inverse matrix")
    return(i)
  }else{
    data <- x$get()
    i <- x$setInverse()
    return(i)
  }
}

##Test:
x <-matrix(1:4,2,2)                   ##It is defined a matrix
specialMatrix <- makeCacheMatrix(x)   ##It is created a special matrix 
cacheSolve(specialMatrix)             ##First execution, It is created the inverse matrix
cacheSolve(specialMatrix)             ##Second execution, It is used the inverse matrix cached
cacheSolve(specialMatrix)             ##Third execution, It is used the inverse matrix cached