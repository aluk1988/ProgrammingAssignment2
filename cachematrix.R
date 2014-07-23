## Functions to compute the matrix inverse of an invertible matrix and then cache the result. If it exists in cache then 
## the inverse is retrieved from this instead of being re-calculated.

##makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
            x_inv <- NULL      # remove x_inv as we have called makeCacheMatrix
            set <- function(y) # a function set which sets x from y and makes x_inv NULL in cache
              {
                 x <<- y ##in the call x$set we cache and assign x from y
                 x_inv <<- NULL ##we null the inverse result
              } 
            get <- function() {x} #this retrieves the current value for x held in the function class makeCacheMatrix
            setinv <- function(my_inv=matrix()) {x_inv <<- my_inv} #caches the inverse if we set it
            getinv <- function() {x_inv} #outputs the inverse from the cache if it exists
            list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv) # This is the output of calling the function makeCacheMatrix
                                  # It is a list of the four function
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'
          x_inv <- x$getinv()  # retrieve the current inverse
          if(!is.null(x_inv)) #test if it is null (it hasn't been cached, else retrieve)
          {
            message("getting cached inverse matrix")
            return(x_inv) #retrieves and prints the value of m if cached and exists function
          }
          data <- x$get() # saves a temporary data matrix from x
          x_inv <- solve(data, ...) # calculates inverse
          x$setinv(x_inv) #sets the inverse to the cache as x_inv
          x_inv # returns x_inv
}
