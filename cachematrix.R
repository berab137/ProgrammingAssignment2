## These functions allow the creation and storage of a matrix as an object of 
## the first function. Because it uses a list to assign labels to it's internal
## functions, the second function can pull info from and store info to the same
## variables. As a result, the functions together can create and store a matrix, 
## and then compute the inverse of that matrix. If the inverse has already been
## computed, it will retrieve that inverse, instead of recomputing it. 


## The first function can create or set a matrix and cache it's inverse if it has 
## been inputed to the second function.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y                        ## assigns value of y to object in parent (x)
        inv <<- NULL                   ## replaces value of inv with NULL if a new matrix is set()
    }
    get <- function() x                ## retrieves x from parent env of makeCacheMatrix
    setinverse <- function(inverse) inv <<- inverse   ## assigns input arg (inverse) to value of inv in parent environment
    getinverse <- function() inv                  ## getter for inverse, finds inv and retrieves value
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ## names each function defined above so they can be retrieved in subsequent functions
}



## Return a matrix that is the inverse of the 'cached_matrix' object of makeCacheMatrix. If it has been returned previously, it will provide it from memory. 

cacheSolve <- function(cached_matrix, ...) {     
    inv <- cached_matrix$getinverse()           ##retrieve inverse from cached_matrix
    if(!is.null(inv)) {                 ## check for valid cached inverse and return to parent env
        message("getting cached data")    
        return(inv)         
    }
    data <- cached_matrix$get()         ## if above is FALSE, stores matrix from cached_matrix to 'data'
    inv <- solve(data)                  ## compute the inverse of 'data'
    cached_matrix$setinverse(inv)    ## set the inverse in the input object
    inv                  ## returns value of inverse to parent env by printing it
}

