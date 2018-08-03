## Put comments here that give an overall description of what your
## functions do:

## DA_comments:As per the programming assignment 2 prompt, 
## matrix inversion is costly computationally. 
## Therefore, utilizing the example that was provided by the 
## Course instructors for calculating the means of a vector, I created makeCacheMatrix and
## cacheSolve. These two functions cache are used to create a special object that stores a matrix
## and cache's its inverse (assuming the matrix is invertible).


## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to:

# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y #stores the matrix x in the cache
        inv <<- NULL #stores inv in cache
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function:
## The following function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see 
## if the inverse has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, 
## it calculates the inverse of the matrix and sets the inverse matrix in the 
## cache via the setinverse function.


cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) { #checks if the inverse is already stored in the local environment
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...) #calculates inverse if it hasn't already been stored in the cache.
    x$setinverse(inv)
    inv
}

# To test that the above function works, one can use the matrix
# test<-matrix(c(2,2,3,2),nrow = 2, ncol = 2, byrow= FALSE, dimnames =NULL )
# test is invertible as det(test) is non-zero
# So, then I define test_1<-makeCacheMatrix(test)
# and cacheSolve(test_1)
# cacheSolve(test_1) will yield the inverse of test.
