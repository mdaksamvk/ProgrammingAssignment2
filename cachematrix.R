makeCacheMatrix <- function(x = matrix()) {
  ## @x:function creates a special "matrix" object that can cache its  inversE
  
  inv = NULL##NULL matrix is created
  set = function(y) {
    # <<- operator which used to assign a value to an object in an environment that is different from the current environment
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  
  if (!is.null(inv)){
    # get it from the cache
    message("getting cached data")
    return(inv)
  }
  
    mat.data = x$get()
  inv = solve(mat.data, ...)
  
   x$setinv(inv)##fetching inverse
  
  return(inv)
}