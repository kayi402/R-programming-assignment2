## My functions can be used to caching the inverse of a matrix

## This function creates a special "matrix" object 
makeCacheMatrix <- function(x = matrix()) {
        
        a <- NULL
        set <- function(y) {
                x <<- y
                a <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) a <<- solve
        getsolve <- function() a
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
cacheSolve <- function(x, ...) {
        
        cacheSolve <- function(x, ...) {
                a <- x$getInverse()
                if(!is.null(a)){
                        message("getting the inversed result")
                        return(a)
                }
                data <- x$get()
                a <- solve(data,...)
                x$setInverse(a)
                a
        }
        
}
