## Put comments here that give an overall description of what your
## functions do

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
inv <- NULL
set <- function(y) 
{
x <<- y
inv <<- NULL
}
get <- function() x
setinverse <- function(inverse) 
inv <<- inverse
getinverse <- function() inv
list(set = set,get = get,setinverse = setinverse,getinverse = getinverse)
}


##The purpose of this function is to compute the inverse of the matrix created above 
##If the original matrix has not changed, then it shall retrieve the values from the cached value
##thus saving time on computations


cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'

inv <- x$getinverse()
if (!is.null(inv)) 
{
message("getting cached data")
return(inv)
}
mat <- x$get()
inv <- solve(mat, ...)
x$setinverse(inv)
inv
}