## Caching the Inverse of a Matrix

## The following creates a special "matrix" object, which is really a list containing a funciton to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the inverse of a matrix
## 4. get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        invM <- NULL
        set <- function(y){
                x <<- y
                invM <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) invM <<- inverse
        getInverse <- function() invM
        list(set = set,
              get = get,
              setInverse = setInverse,
              getInverse = getInverse)
}


## The following function computes the inverse of the special "matrix" returned by makeCachMatrix above. If the inverse has already
## been calcuated(and the matrix has not change), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invM <- x$getInverse()
        if (!is.null(invM)){
                message("getting cached data")
                return(invM)
        }
        data <- x$get()
        invM <- solve(data, ...)
        x$setInverse(invM)
        invM
}
