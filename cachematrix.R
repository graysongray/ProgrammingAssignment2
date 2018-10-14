# This all functions will calculate Inverse Matrix and cache it

### Example Functions do:
# mat <- matrix(c(3, 8, 4 ,6), nrow=2, ncol=2)
# myMatrix <- makeCacheMatrix(mat)
# cacheSolve(myMatrix)


## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        # Initialize Inverse Matrix = null
        invM <- NULL
        
        # Set Matrix Value (Setter)
        set <- function(y) {
            x <<- y
            invM <<- NULL
        }
    
        # Get Matrix Value (Getter)
        get <- function() x
        
        # Set Inverse Matrix Value (Setter Inverse)
        setinv <- function(inv) invM <<- inv
        
        # Get Inverse Matrix Value (Getter Inverse)
        getinv <- function() invM
        
        # Set function to be accessible easier by returning as list
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}



## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        #Get Inverse Matrix
        invM <- x$getinv()
        
        ## If Inverse Matrix already exist, then return cached data
        if(!is.null(invM)){
            message("getting cached data")
            return(invM)
        }
        
        ## If Inverse Matrix not exist, then calculate & set the Inverse Matrix
        # Get Matrix
        data <- x$get()
        
        # Calculate Inverse Matrix
        invM <- solve(data)
    
        # Set Inverse Matrix
        x$setinv(invM)
        
        # Return Iverse Matrix
        invM
}
