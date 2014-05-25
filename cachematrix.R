## Creates a special object containing methods for getting/setting a matrix,
## as well as getting/setting the cached value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    ## Initialize cache with NULL
    i <- NULL
    
    ## Sets the value and clears the cache
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## Gets the value
    get <- function() x
    
    ## Stores the inverse value in the cache
    setinverse <- function(inverse) i <<- inverse
    
    ## Returns the cached value (if any)
    getinverse <- function() i
    
    ## Function returns a list contaning the methods defined above
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Accepts a matrix, then checks if there is a cached value of the inverse matrix.
## If not found, calculates the inverse and stores it in the cache.
cacheSolve <- function(x, ...) {
    ## Trying to get the cached inverse value first
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## If we are here, it means no cache value was found
    ## Get the value, calculate the inverse and store it in the cache
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    
    ## Return the calculated inverse value
    i
}
