### Assignment: Caching the Inverse of a Matrix
##  Creating functions that cache the inverse of a matrix.

        
##  This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        # initialize the stored inverse value to NULL
        m <- NULL 
        # create function that sets the input matrix 
        set <- function(y) { 
                x <<- y
                m <<- NULL
        }
        # create function that gets the input matrix
        get <- function() x
        # create function that sets the inverse matrix
        setinv <- function(inv) m <<- inv
        # create function that gets the inverse matrix
        getinv<- function() m # should be a matrix 
        # creat list with the 4 functions created above 
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) { # if inverse has already been calculated, the function gets the matrix from the cached data
                message("getting cached data")
                return(m)
        }
        ## otherwise, we need to calculate the inveser of the input matrix using solve
        data <- x$get() # gets input matrix 
        m <- solve(data, ...) # calculates the inverse martix of input matrix
        x$setinv(m) # sets m as the inverse matrix 
        m      # returns the inverse matrix 
}

### END
