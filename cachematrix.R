## cachematrix.R contain two main function(makeCacheMatrix(),cacheSolve()) which calculate the inverse of a matrix and 
## if same value matrix is passed next time for inverse calculation then instead of computing inverse of a matrix, 
## it get the result from the cache memory( Here it's m) and throw the output on monitor with message. 

## makeCacheMatrix function : It store a "matrix" object that contain the value of a matrix and inverse of earlier matrix.

makeCacheMatrix <- function(x = matrix())
    {
        m <- NULL    # Assign null value to m when makeCacheMatrix is called
        set <- function(y) { # It will store the provided matrix and set m as null value
                          x <<- y     # get the value of x from the parent context
                          m <<- NULL 
                           }
        get <- function() x # set new matrix value to x in a subfunction "get" to store as a matrix  
        setsolve <- function(solve) m <<- solve # set the inverse matrix value in m; which get the value from cacheSolve 
        getsolve <- function() m      # store the inverse matrix value in m 
        list(set = set, get = get,    # Return the list of the subfunctions used
             setsolve = setsolve,
             getsolve = getsolve) 

    }


## cacheSolve function 
## 1. Take the argument(x) value from above function and proceed further
## 2. check the value of m (from above function) whether its null or having values. 
## 3. Calculate the inverse of a matrix and send the cache matrix to above subfunction(setsolve)

cacheSolve <- function(x, ...) # Pass the argument x and ... used when argument (x) to call other functions 
                                # if we don't prespecify functions names.
             {
              m <- x$getsolve()  # Get the value of m using getsolve subfunction of agrument x(Defined in makeCacheMatrix)
                      if(!is.null(m))  # If value of m is not null;return the value of m (Inverse matrix value) 
                                        #with quoted message on console
                       {
                         message("getting cached data")
                         return(m)     # if not null then return the value of m i.e, end of the code
                       }        
                     data <- x$get()     # As value of m is null then pass new matrix to object "data"
                     m <- solve(data, ...) # Compute the inverse of matrix and store into m
                     x$setsolve(m)         # Assign m value into setsolve function, so that it would be squirreled away 
              m                  # Display inversed matrix on console
             }

