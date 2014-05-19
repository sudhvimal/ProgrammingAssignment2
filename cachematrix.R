## cachematrix.R containing 2 main function(makeCacheMatrix(),cacheSolve()) which calculate the inverse of a matrix and 
## if same matrix is passed next time then instead of computing inverse of same matrix, it get the result from 
## the cache memory( Here it's m) and throw the output on monitor with message. 

## makeCacheMatrix function : It creates a "matrix" object that contain the value of matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL    # Assign null value to m when makeCacheMatrix is run
        set <- function(y) { # It will store the provided matrix and set m with null value
                x <<- y     # get the value of y from the parent context, 
                            # here it's getting from get function value
                m <<- NULL 
        }
        get <- function() x # set the value of subfunction "get" for argument x which can be called in other function 
        setsolve <- function(solve) m <<- solve # set the inverse matrix value in m; which get the value from cacheSolve 
        getsolve <- function() m      # store the inverse matrix value of m in subfunction "getsolve"
        list(set = set, get = get,    # Return the list of the subfunctions used
             setsolve = setsolve,
             getsolve = getsolve) 

}


## cacheSolve function 
## 1. Take the argument(x) value from above function and proceed further
## 2. check the value of m (from above function) whether its null or having values. 

cacheSolve <- function(x, ...) # Pass the argument x and ... used when argument (x) to call other functions 
                                # if we don't prespecify functions names.
             {
              m <- x$getsolve()  # Get the value of m using getsolve subfunction of agrument x(Defined in makeCacheMatrix)
                      if(!is.null(m))  # If value of m is not null;return the value of m (Inverse matrix value) 
                                        #with quoted message on console
                       {
                         message("getting cached data")
                         return(m)     # if not null then return the value to m i.e, end of the code
                       }        
                     data <- x$get()     # As value of m is null then pass new matrix to object "data"
                     m <- solve(data, ...) # Compute the inverse of matrix and store into m
                     x$setsolve(m)         # Assign m value into setsolve function, so that it would be squirreled away 
             m                  # Display inversed matrix on console
            }

