## Annie Wong
## Assignment 2- Matrix inverse cache storing function 
## First I create a function acting as make a special "vector" list contains 
## a bunch of functions of set the value of the matrix, get the value of matrix,
## set the inverse of matrix, and get the inverse of the matrix.
## Second I use the Cache inverse function to get the inverse of the matrix.
## If the matrix is not changing or the cache already have the inverse of the 
## current matrix, then I retrieve the inverse from the cache immediately.
## If the matrix is new and the computation is not done yet, I call the solve
## function to compute its inverse and set as the new inverse value in the Cache.


## This function is to make the vector and get a list of functions used to 
## set and get the matrix and its inverse values respectively.

makeCacheMatrix <- function(x = matrix()) {
    inv <-NULL
    set <- function(y) {
        x <<-y
        inv <<-NULL
    }
    get <- function() x
    setinv <-function(inverse) inv <- inverse
    getinv <- function() inv
    list(set =set, get= get, setinv =setinv, getinv <- getinv)
}
##Through the above function one can get a list containing 4 functions about 
## the matrix

## This function is to get the Cache inverse of the matrix that first check 
## whether the inverse exists in the cache and when the current matrix is not 
## changing it returns the Cache inverse value. If the matrix is new, it 
## computes the inverse again and stores into the Cache and set it as the new
## inverse value.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting the cached data")
        return(inv)
    }
    data <- x$get()
    #This solve function is to compute the inverse of a square invertible matrix
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

