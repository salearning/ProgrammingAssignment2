## The following functions allow to store a matrix and its inverse in a
## different environment from the one in use. In this way they allow to save
## time and energy by storing the inverse and retrieving it from the cache every
## time it is needed instead of calculating it again.

## This function allows to create a list of four functions.
## The first two functions create a matrix and store it in another environment.
## The second two functions do the same with the inverse of the same matrix.
makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  get<-function() x 
  setinv<-function(solve) inverse<<-solve(x)
  getinv<-function() solve
  list(set=set, get=get,setinv=setinv,getinv=getinv)
}


## This function allows to calculate the inverse of the input matrix x.
## Before computing the inverse, the function checks whether the result has
## already been calculated previously and stored in the cache. If the inverse
## is present in the cache the function returns the existing value, otherwise 
## it computes the inverse from scratch with the function "solve".

cacheSolve <- function(x) {
        inverse<-x$getinv()
        if(!is.null(inverse)){
          message("getting cached data")
          inverse
        }
        data<-x$get()
        inverse<-solve(data)
        x$setinv(inverse)
        inverse
}
