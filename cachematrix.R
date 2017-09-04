## makeCacheMatrix creates an object that can store a matrix and the
## inverse of that matrix. To calculate the inverse the  cacheSolve function 
## can be used, which will calculate the inverse the first time it is called
## and then store the inverse for further use.

## makeCacheMatrix creates an object that can store (and return)
## a matrix and the inverse of the matrix. The matrix is stored with the 
## function $set and retrieved with $get. The inverse is stored with the
## function $setinv and retrieved with $getinv.

makeCacheMatrix <- function(mtrx = matrix()) {
    
    inv_mtrx <- NULL
    set <- function(mx) {
        mtrx <<- mx
        inv_mtrx <<- NULL
    }
    get <- function() mtrx
    setinv <- function(im) inv_mtrx <<- im
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
    if(!is.null(inv_m)) {
        message("getting cached data")
        return(inv_m)
    }
    data <- mtrx$get()
    
    inv_m <- solve(mtrx$get())
    
    mtrx$setinv(inv_m)
    inv_m
    
}
