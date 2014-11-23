## These pair of functions will be used to optimize matrix inversions by caching the inverse of a matrix rather than 
## computing for it repeatedly
## The first function, makeCacheMatrix, will create a special matrix 'x' and cache its inverse 'm'
## The second function, cacheSolve, will return the inverse of the matrix, if it was previously cached, or compute it  

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

  ## Nothing is cached yet, so set default to NULL
  m <- NULL
  
  ## Set the value of the Matrix
  set<-function(y){
    x <<- y
    m <<- NULL
  }
  
  ## Get the value of the Matrix
  get <- function() x
  
  ## Set the value of the Inverse of matrix
  setmatrix <- function(solve) m<<- solve
  
  ## Get the value of the Inverse of matrix  
  getmatrix <- function() m
  
  ## Returns a list of names of each function
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
  
}


## This function will calculate the inverse of the matrix created from the makeCachematrix function

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of x
  m <- x$getinverse()

  ## If there is a cached value of the inverse matrix, then it returns it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }

  ## If the cached value of the inverse matrix is null, then R will get the referenced matrix
  matrix <- x$get()
  
  ## This will calculate value of the inverse matrix
  m <- solve(matrix, ...)
  
  ## Then set the value of the inverse matrix 
  x$setinverse(m)
  
  ## Returns the inverse matrix
  m

}

