## These functions save computation time by utilizing a previously computed matrix inverse if it exists


## This function produces a list of 4 functions.  
## it is used to cache a value that can be call in other environments
## set globally assigns the matrix to x.  
## get returns x.
## setinv globally assigns the matrix inverse to m
## getinv returns m

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(minv) m <<- minv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function calculates the inverse of a matrix x.
## If the matrix had previously been calculated and stored in m
## it will return the value of m rather than recalculating.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

mymat<-matrix(sample(1:9,16,TRUE),4,4)
checkmakematrix<-makeCacheMatrix(mymat) 
cacheSolve(checkmakematrix) 
mymat<-matrix(sample(1:9,36,TRUE),6,6)
cacheSolve(checkmakematrix) 
checkmakematrix$setinv(NULL)
cacheSolve(checkmakematrix) 
checkmakematrix$setinv(NULL)
checkmakematrix<-makeCacheMatrix(mymat) 
cacheSolve(checkmakematrix) 

#ugh, why isn't the getting pushed!
