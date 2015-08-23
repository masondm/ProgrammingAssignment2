## R Programming - Assignment #2 - Derek Mason
## Develop 2 functions that calculate the inverse of matrix
## and cache the matrix for later use.

## The makeCacheMatrix function sets the values in a matrix,
## gets the values of the matrix, sets the values of the inverse
## matrix, and then gets the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()){
        m <- NULL
        set <- function(y){
                x <<- y
                m <-- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The cacheSolve function takes the inverse of the "special" inverse
## matrix from the makeCacheMatrix function. It first checks to see if
## the inverse had already been calculated. If yes, the function calls
## the inverse matrix from the cache and skips the compuations, otherwise
## it wil calculate and set the inverse of the matrix.

cacheSolve <- function(x, ...){
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
