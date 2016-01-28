## This set of functions create a special matrix that can cache its inverse.

## The first function (makeCacheMatrix()) creates a special "matrix", which is really a list containing a function to:
## (1) set and (2) get the value of the vector, (3) set and (4) the value of the inverse  

makeCacheMatrix <- function(x = matrix()) {

        i <- NULL
        
        set <- function(y) { # set the matrix
                
                x <<- y
                
                i <<- NULL
                
        }
        
        get <- function() x # get the matrix
        
        seti <- function(inverse) i <<- inverse # set the inverse
        
        geti <- function() i # get the inverse
        
        list(set = set, get = get, seti = seti, geti = geti)
        
}


## Computes the inverse of the special "matrix" returned from makeCacheMatrix(). Then, it retrieves the inverse from the cache, or calculates it (if the inversed wasn't calculated before).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$geti()
        
        if(!is.null(i)) { # check if the inverse was calculated previously
                
                message("getting cached data")
                
                return(i) # if it had been calculated, retrieves from the cache
                
        }
        
        data <- x$get()
        
        i <- solve(data, ...) # if it wasn't, it calculates now!
        
        x$seti(i)
        
        i
        
}
