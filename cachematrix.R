## makeCacheMatrix creates a matrix with a cache that its inverse can be stored in. If the matrix inverse has been solved previously,
## cacheSolve will use the cached inverse value, if it has not been cached previously, it will then cache the value for future reference.



## MakeCacheMatrix creates a special matrix with 4 additoinal functions to set,get,setInverse & getInverse.
## it returns a list of the 4 funcitons that get utilised by the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve takes the special matrix object created through the makeCacheMatrix function. It first acesses
## m within the special matrix object to check if it is NULL (and thus if its inverse has been previously cached!).
## If its NULL, the matrix data is accessed and inversed using solve() and saved to 'm' (which is stored outside cacheSolve's 
## environment), thus effectivly chaching the Inverse in 'm'. If the same special matrix object is put through cacheSolve() 
## again, 'm' wont me NULL so it will access the cached inverse instead of solving it again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
