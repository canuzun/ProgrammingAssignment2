makeCacheMatrix <- function(x) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setInverse <- function(inversedMatrix) im <<- inversedMatrix
    getInverse <- function() im
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
    im <- x$getInverse()
    if(!is.null(im)) {
        message("getting cached inverse matrix")
        return(im)
    }
    matrix <- x$get()
    im <- solve(matrix)
    x$setInverse(im)
    message("setting cached inverse matrix")
    im
}




#####
#mt = matrix(data=1:4, ncol=2, nrow=2)
#matrix <- makeCacheMatrix(mt)
##str(matrix)
#cacheSolve(matrix)
#cacheSolve(matrix)
##matrix <- makeCacheMatrix(mt)
#cacheSolve(matrix)
#cacheSolve(matrix)
