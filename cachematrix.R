## makeCacheMatrix creates an object that can store (and return)
## a matrix and the inverse of the matrix. The matrix is stored with the 
## function $set and retrieved with $get. The inverse is stored with the
## function $setinv and retrieved with $getinv.

makeCacheMatrix <- function(mtrx = matrix()) {
    
    # initialize the inverse matrix to NULL
    inv_mtrx <- NULL
    
    # the set function stores the matrix and sets the inverse to NULL
    set <- function(mx) {
        mtrx <<- mx
        inv_mtrx <<- NULL
    }
    
    # the get function returns the matrix
    get <- function() mtrx
    
    # the setinv function sets the inverse matrix
    setinv <- function(im) inv_mtrx <<- im
    
    # the getinv function returns the inverse matrix
    getinv <- function() inv_mtrx
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve takes an object of the type created by makeCacheMatrix and
## 1. returns the inverse of the matrix stored in that object, 
## 2. calculates and sets the inverse component of the object 
##    if it is not already set

cacheSolve <- function(mtrx, ...) {
    
    inv_m <- mtrx$getinv()
    
    # Check if the inverse is already stored, if so return it
    if(!is.null(inv_m)) {
        message("getting cached data")
        return(inv_m)
    }
    
    # If the inverse is not already stored, then retreive the matrix and then
    # calculate, store and return the inverse.
    inv_m <- solve(mtrx$get())
    mtrx$setinv(inv_m)
    inv_m
    
}
