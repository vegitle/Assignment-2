makeCacheMatrix <- function(x = matrix()) {
    i <- NULL # set i (inverse) to null as no inverse data to begin with
    set <- function(y) { # stores matrix into variable, when new matrix is created, i will be set back to null as previous inverse will be wrong
        x <<- y
        i <<- NULL
    }
    get <- function() x # returns matrix
    setinverse <- function(solve) i <<- solve 
    getinverse <- function() i # returns inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) # list function of 4 variables
}
cacheSolve <- function(x, ...) {
    i <- x$getinverse() # sets inverse from the getinverse function
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    } # if inverse is not null, display message and show matrix inverse
    data <- x$get() # if null set matrix into variable
    i <- solve(data) # inverse the matrix and place in variable
    x$setinverse(i) # use set inverse variable as i
    i
}
