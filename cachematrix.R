## The following functions calculates the inverse of a matrix and stores the
## result in cache memory if needed to be called again

x <- matrix(1:16, 4, 4)     ## creates a "dummy" matrix for practice calculations

## Function to calcualte matrix inverse
makeCacheMatrix <- function (x = matrix()) {
        
        ## creates a variable to store the inverse of the matrix "x"
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        
        ## functions to retrieve data from x and calculate the inverse of the matrix
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m 
        
        ## list containing the contents and environments for the above functions
        ## for use in the cacheSolve function
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Function to call check if matrix inverse exists and calls from cahce if so
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        
        ## if-statement to check if inverse of matrix exists from previous function
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ##calculates and returns the inverse of the x matrix if not retrievable from cache
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}