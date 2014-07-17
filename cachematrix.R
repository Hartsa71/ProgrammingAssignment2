## With chachematrix.R you can save time if you have to recalculate inverse of the 
## same matrix multiple times. This implementation returns already calculated 
## inverse of the matrix is such exists

## makeCacheMatrix will create a cached matrix object with setters and getters
## inverse of matrix is stored to 'inv' and matrix itself to 'mat'
makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  set <- function(m) {
    mat <<- m
    inv <<- NULL
  }
  get <- function() mat
  setInv<- function(i,...) inv <<- i
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)}


## cacheSolve function uses makeCacheMatrix object as argument and
## if there is already calculated inverse of the matrix it is
## returned instead of recalculating it again
cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- mat$getInv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  m <- mat$get()
  inv <- solve(m,...)
  mat$setInv(inv)
  inv
}

## for testing purposes...
## m<-matrix(c(4,2,7,6), nrow=2)
## myM <- makeCacheMatrix(m)
## cacheSolve(myM)
## cacheSolve(myM)
