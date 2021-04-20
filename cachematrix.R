## There is an object called special 
## matrix that can inverse the cache.
## In this computation it is all
## about the Matrix inversion which
## could result to a high cost.
## It also utilizes the computation
## of the inverse matrix over and over.
## There are also usage on creating
## special object of pairing the functions
## that stores inverse matrix and caches.

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## After the special matrix, there should
## be another important function named 
## cacheSolve where it computes the 
## recurred feedback of the makeCacheMatrix.
## In recovering the cache inverse it 
## needs to be the same as it is in 
## examining the special matrix
## that is inverse and computed instrument.

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cache data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}