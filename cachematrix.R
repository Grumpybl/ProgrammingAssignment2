## Coursera - R Programming Course - Drs. Peng, Leek, Caffo

## Programming Assignment 2: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it
## repeatedly. Below are two functions that cache the inverse of a matrix.

## The first function `makeCacheMatrix` creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      ## set the value of the matrix
      m<- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      ## get the value of the matrix
      get <- function() x
      
      ## set the inverse of the matrix
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      
      ## get the inverse of the matrix
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## The second function ''cacheSolve' computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then the
## `cachesolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      ## get the inverse of the matrix        
      m <- x$getinverse()
      
      ## check if there is the matrix   
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      ## if not: get the inverse of the matrix   
      data <- x$get()
      m <- solve(data, ...)
      ## set the inverse of the matrix 
      x$setinverse(m)
      m
}