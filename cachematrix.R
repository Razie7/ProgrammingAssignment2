## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
i <- NULL                  #intiating of null value for the the inverse variable (i)
        set <- function(y) {
                x <<- y            ## insert new matrix and pass it to x
                i <<- NULL      ## reset the Inverse variable because of the new inserted matrix
        }
        get <- function() x     #
        setinverse <- function(inverse) i <<- inverse  # assign i variable the inverse matrix
        getinverse <- function() i     # show the inversed Matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}
##basically this function return result list of the four sub-functions (set,get, setinverse and getinverse)
# so this list could be accessd by the cacheSolve function

cacheSolve <- function(x, ...) {

  i <- x$getinverse()   
        if(!is.null(i)) {   #if the inverse of the matrix has been calculated before then
                message("getting cached inversed matrix")   # return the value from cach
                return(i)
        }
        matrix <- x$get()      # assign the input matrix to the variable called (matrix) 
        i <- solve(matrix)    # solve fucntion computes the inverse of the input matrix, the result assigned to (i)
        x$setinverse(i)       # update the new result to i in the makecachematrix function so when getinverse() retrieves the proper value.  
        i    ## Return of the inverse matrix of x.
}
