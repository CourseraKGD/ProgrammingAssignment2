## These functions provide for the efficient calculation of inverse matrixes
## by caching results and serving them up again, if appropriate
## (assumes all matrixes are square)

## Creates a special vector, that is really a cache with 4 functions  
## 1.  set a matrix
## 2.  get a previously set matrix
## 3.  set the inverse of the matrix
## 4.  get the inverse of the matrix

makeCacheMatrix <- function(x = vector()) {
    ## no inverse set yet    
    m <- NULL
    
    ## new matrix so set inverse to null
    set <- function(y) {
        # see if the matrix is square
        if (nrow(y) != ncol(y)) {
            message("has to be a square matrix")
            return(FALSE)
        }
        
        # see if this is the same matrix
        if (identical(x, y)) {
            message("identical to set matrix")
            return(FALSE)
        }
        
        # set matrix, clear inverse
        x <<- y
        m <<- NULL
        
        return(TRUE)
    }
    
    ## return matrix
    get <- function() x
    
    ## set the inverse matrix
    setinverse <- function(inverse) m <<- inverse
    
    ## get the inverse matrix
    getinverse <- function() m
    
    ## return a list of accessor functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}

## Calculate the inverse of the matrix seup with the function above. 
## If a valid inverse has already been calculated, the cached inverse
## is returned, otherwise a new inverse is calculated and cached 

cacheSolve <- function(x, ...) {
    ## get the inverse
    m <- x$getinverse()
    
    # return cache data if available
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # get matrix
    data <- x$get()
    
    # calculate inverse
    m <- solve(data)
    
    # cache inverse
    x$setinverse(m)
    
    # return calculated inverse
    m    
}
