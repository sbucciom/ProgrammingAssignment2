

## This function creates a special "matrix" object that can cache its inverse



##  Like all the cache contains the accessories get and set, realized using closures function, 
##  to manipulate the data to manage

makeCacheMatrix <- function(x = matrix())
{
        ix <- NULL
        
        set <- function(y) {
                x <<- y
                ix <<- NULL
        }
        get <- function() x
        setinverse <- function(inversem) ix <<- inversem
        getinverse <- function() ix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}



##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## It allows you to save the inverse of a matrix by not recalculate if already done.

cacheSolve <- function(x, ...)
{
        ## Return a matrix that is the inverse of 'x'
        ix <- x$getinverse()
        if(!is.null(ix)) {
                message("getting cached data")
                return(ix)
        }
        data <- x$get()
        ix <- solve(data, ...)
        x$setinverse(ix)
        ix
}
