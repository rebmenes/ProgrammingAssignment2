## The goal of the following two functions is to cache the inverse of a matrix.
## Since matrix inversion is an intensive task, we prefer not to repeat it unless
## the matrix is changed. In order to cache the inverted matrix we 
## prepare a caching variable that maintains the matrix properties including
## the inverted matrix. 

## The first function "makeCacheMatrix" maintains the cached value of the inverted
## matrix, so long as it is not changed. "makeCacheMatrix" returns 4 functions 
## with roles of setting ("set" and "setinv") or getting ("get" and "getinv")
## both the original matrix and its inverse.
## these functions are later used by cacheSolve as input.


makeCacheMatrix <- function(x = matrix()) {
  ##In order to be sure that if the matrix is changed the inverse matrix in the cache
  ##is removed we set the value to NULL.
  
  inv<-NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## "cacheSolve" has the role to either compute the inverse matrix or, if it already has
## been computed, provide the version that has been saved in the cache. "CacheSolve"
## takes as input the cached matrix which is the output of "makeCacheMatrix". If a 
## cached version of the inverted matrix does not exist it will calculate it and will
## update the cached version by changing the value using the "setinv" function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse of matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
