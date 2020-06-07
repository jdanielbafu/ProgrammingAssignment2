## the function makeCacheMatrix creates a list containing a function to
# 1. set the values of the matrix
# 2. get the values of the matrix
# 3. set the inverse matrix
# 4. get the inverse matrix

makeCacheMatrix <- function(x = matrix()){
    inversematrix <- NULL
    #assign values to a objects that can be retrieved
    set <- function(y) {
        x <<- y
        inversematrix <<- NULL
    }
    get <- function () x
    #Create a function to set the inverse in inversematrix
    setInverse <- function(inverse) inversematrix <<- inverse
    
    getInverse <- function() inversematrix
    #create the list of the values to get it in the next function
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## this function evaluates if theres is a solution already stored
## next it return the value or calculate it

cacheSolve <- function(x, ...){
    #get the value for the inverse matrix
    inversematrix <- x$getInverse()
    #First check if there is a value cached, if it exist returns this value
    if (!is.null(inversematrix)){
        message("getting cached data")
        return (inversematrix)
    }
    #if the value does not exist, calculate it and stored it in inversematrix
    mat <- x$get()
    inversematrix <- solve(mat, ...)
    x$setInverse(inversematrix)
    #assign the result to cacheSolve
    inversematrix
}

