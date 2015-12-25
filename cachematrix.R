##Below are two functions that are used to create a special object that stores matrix 
##and cache's inverse matrix.  
##this function 
##makeCacheMatrix creates a special "vector", which is really a list containing a function to
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the matrix inverse
##    get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
     m<- NULL
     set <- function(y) {
       x<<- y
       m <<- NULL
 }
     get <- function() x
     setinverse <- function(inverse) m<<- inverse
     getinverse <- function() m
     list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
 }

##The following function calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse matrix has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation.
## Otherwise, it calculates the inverse matrix and sets the inverse of the matrix in the cache via the setinverse function.

cacheSolve<- function(x,...) {
 m<- x$getinverse()
 if (!is.null(m)) {
    message("getting cached data")
    return(m)
 }
 data<-x$get()
 m<-solve(data,...)
 x$setinverse(m)
 m
 }



