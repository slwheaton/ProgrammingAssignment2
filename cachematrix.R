#--Coursera - R Programming - Week 3 Assignment

#--Create matrix inverse to cache
makeCacheMatrix <- function(x = matrix()) {
     matrixinv <- NULL
     set <- function(y) {               
          x <<- y
          matrixinv <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) matrixinv <<- inverse
     getinverse <- function() matrixinv
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#--Retrieve cached matrix inverse
cacheSolve <- function(x, ...) {
     matrixinv <- x$getinverse()
     if(!is.null(matrixinv)) {
          message("getting cached data")
          return(matrixinv)
     }
     data <- x$get()
     matrixinv <- solve(data, ...)
     x$setinverse(matrixinv)
     matrixinv
}
