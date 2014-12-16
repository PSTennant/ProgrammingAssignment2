## These functions take the inverse of square matrix and cache that inverse

## This function caches a matrix object for inverting in the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
                   inv <- NULL
                   get <- function() { x }
                   setinv <- function(invert) { inv <<- invert }
                   getinv <- function() { inv }
                   list(get = get, setinv = setinv, getinv = getinv)

}

## This function take the inverse of the matrix from the makeCacheMatrix function
## returns the inverse of it and stores the inverse for later return

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
              inv <- x$getinv()
                   if(!is.null(inv)){ 
                   message("getting cached data")
                   return(inv)
                   }
              data <- x$get()
              inv <- solve(data, ...)
              x$setinv(inv)
              inv
}


#Creating a matrix object
mymatrix <- matrix(c(1:3, 5, 9, 14, 17, 21, 20:27), nrow = 4)
#Saving the matrix for inverting
cash1 <- makeCacheMatrix(mymatrix)
#Inverting the matrix saved in cash1
cacheSolve(cash1)


