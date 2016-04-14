## Caching the inverse of a matrix

## Creating a special matrix object that can cache its inverse:-makeCacheMatrix function

makeCacheMatrix <- function(x = matrix())
{
 inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Computation of the inverse of the matrix created by the makeCacheMatrix function. If the inverse has already been calculated then the 
##inverse will be retrieved from the cache.:- cacheSolve function

cacheSolve <- function(x, ...) {
inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv

        ## Return a matrix that is the inverse of 'x'
}
