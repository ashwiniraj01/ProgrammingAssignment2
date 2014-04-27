## The following two functions are used to caclulate and cache a matrix input,
## and retrieve the same cached inverse matrix when required instead of 
## recalculating it.
## In case it is not found in the cache, they are calculated and printed.

## Function 1 : makeCacheMatrix()
## This function takes a matrix(a square invertible matrix) as input and 
## creates a special matrix to cache its inverse.
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)

}


## Function 2. cacheSolve()
## The following function calculates the inverse of the special vector created 
## by the above function. If the inverse has already been calculated it 
## retrieves and prints the same. Else, it calculates and prints the inverse.


cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'

        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m

}

## Example Test Cases

## x <- matrix(1:4, nrow=2, byrow=TRUE)
## a = makeCacheMatrix(x)
## cacheSolve(a)
## cacheSolve(a)
## y <- matrix(c(1,0,1,2,4,0,3,5,6), nrow=3, byrow=TRUE)
## b = makeCacheMatrix(y)
## cacheSolve(b)
## cacheSolve(b)

