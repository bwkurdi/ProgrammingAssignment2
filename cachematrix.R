## The purpose of the bellow functions is to create a special matrix, 
## get its inverse and cache it, as to save time if the inverse of aninputted matrix has already been inversed.


## makeCacheMatrix is a function that creates a special matrix object and gets its inverse.
## The aforementioned function has 4 functions inside of it. 
##set:setting the value of the matrix
##get:get the value of the matrix
##setreverse:set the value of the inverse 
##getreverse: get the value of the inverse
  makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ##define the cache inv
    set <- function(y) {
      x <<- y ##substitute the value of the matrix in the main function
      inv<<- NULL ## restores to null the value of the inversed matrix inv, because the old value of the old matrix is not needed anymore. The new matrix needs to be inversed again through the function cacheSolve
    }
    get <- function() x ## return the matrix x
    setreverse<- function(reverse) inv <<-reverse ## store the value of the input in a variable inv into the main function and(set the value of the inverse )
    getreverse <- function() inv  ## return the value(get the value of the inverse)
    list(set = set, get = get,
         setreverse = setreverse,
         getreverse = getreverse)}
  



  ## cacheSolve returns the inverse of a matrix,
  ##but it first checks if the inverse of a give matrix has already 
  ## been calculated if so it gives a message "getting cached reverse matrix" and returns the cached inverse
  ## if not it calculates the inverese and present the input caching it. This function uses 
  ## the functions stored in the main 
  ##function  thus we need to subset the main function
  
  
cacheSolve <- function(x, ...){ ## Return a matrix that is the inverse of 'x'
  inv <- x$getreverse()  
  if (!is.null(inv)) { ## verify the value of inv if it is null or if it is stored in the getreverse 
    message("getting cached reverse matrix")## if it is not equal to null then it presents this message and
    return(inv) ## returns the cached value
  } else {
    data<- solve(x$get())## if it is equal to null then it calculates the inverse 
    x$setreverse(data)
    return(inv) ##returns the inversed value
    
  }
}
  