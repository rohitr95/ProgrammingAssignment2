## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) { ## This function creates the cache matrix
    inv <- NULL                             
    set <- function(a) {                    
        x <<- a                             
        inv <<- NULL                        
    }
    get <- function() x                     
    
    setinverse <- function(inverse) inv <<- inverse  ##inverse is being set
    getinverse <- function() inv                     ##inverse is retireved
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
                                                                                  
cacheSolve <- function(x, ...) { ## function to get cached data.
        inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv                     ## output
}
