## For a very large matrix, it may take too long to compute the inverse, 
## especially if it has to be computed repeatedly (e.g. in a loop). 
## If the contents of a matrix are not changing, it may make sense to cache the value of the inverse 
## so that when we need it again, it can be looked up in the cache rather than recomputed. 
## In this Programming Assignment will take advantage of the scoping rules of the R language 
## and how they can be manipulated to preserve state inside of an R object
## Example of usage:
## > mtx <- matrix(-2:1, 2, 2)
## > mtx
##      [,1] [,2]
## [1,]   -2    0
## [2,]   -1    1
## > solve(mtx)
##     [,1] [,2]
## [1,] -0.5    0
## [2,] -0.5    1
## mtxc <- makeCacheMatrix(mtx)
## mtxc$get()
##      [,1] [,2]
## [1,]   -2    0
## [2,]   -1    1
## cacheSolve(mtxc)
##      [,1] [,2]
## [1,] -0.5    0
## [2,] -0.5    1

## makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse. It includes the following functions:
##	set the value of the matrix
##	get the value of the matrix
##	set the value of the inverse of the matrix
##	get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
			m 	<- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setinverse <- function(inverse) m <<- solve
            getinverse <- function() m
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            m <- x$getinverse()
            if(!is.null(m)) {
                    message("getting cached data")
 #                   return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setinverse(m)
            m
}
