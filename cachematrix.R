## cachematrix.R containing 2 main function(makeCacheMatrix(),cacheSolve()) which calculate the inverse of a matrix and 
## if same matrix is passed next time then instead of computing inverse of same matrix, it get the result from 
## the cache memory( Here it's m) and throw the output on monitor with message. 

## makeCacheMatrix function : It creates a "matrix" object that contain the value of matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL    # Assign null value to m when makeCacheMatrix is run
        set <- function(y) {
                x <<- y     # get the value of y from the parent context, 
                            # here it's getting from get function value
                m <<- NULL 
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve) # Return the list of the subfunctions used

}


## cacheSolve function 
## 1. Take the argument(x) value from above function and proceed further
## 2. check the value of m (from above function) whether its null or having values. 

cacheSolve <- function(x, ...) #
             {
              m <- x$getsolve()   # Get the value of m using getsolve subfunction (Defined in makeCacheMatrix)
                      if(!is.null(m))  # If value of m is not null;return the value of m (Inverse matrix value) 
                                        #with quoted message on console
                       {
                         message("getting cached data")
                         return(m)
                       }        
                     data <- x$get()     # As value of m is null then pass new matrix to object "data"
                     m <- solve(data, ...) # Compute the inverse of matrix and store into m
                     x$setsolve(m)         # Assign m value into setsolve function, so that it would be squirreled away 
             m                  # Display inversed matrix on console
            }

