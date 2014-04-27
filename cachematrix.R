##creates a "special matrix" and a list

makeCacheMatrix <- function(x = matrix()) {
        value <- NULL
        set <- function(y) {
                x <<- y
                value <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) value <<- solve
        getInverse <- function() value
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Bizzaro (Inverse) matrix. If already there, returns cached version.

cacheSolve <- function(x, ...) {
        ## Return inverse of x
        value <- x$getInverse()
        if(!is.null(value)) {
                message("Getting data")
                return(value)
        }
        data <- x$get()
        value <- solve(data, ...)
        x$setInverse(value)
        value
}
