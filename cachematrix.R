## Calculating the inverse of a matrix is costly.
## These functions cache the inverse of a matrix
## rather than computing it repeatedly

## makeCacheMatrix creates a special 'matrix' which 
## is really a list containing a function to

## 1. set the value of a matrix
## 2. get the value of a matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <-NULL
        set <- function(y){
                x<<-y
                inv <<-NULL
        }
        get <- function() x
        setinv <- function(inverse) inv<<-inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve computes the inverse of the special 'matrix'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}
