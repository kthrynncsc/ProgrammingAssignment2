makeCatcheMatrix <- function(x= matrix()) {
  k <- NULL  #to initialize k as NULL
  set <- function(y){
    x <<- y
    k <<- NULL
  } 
  get <- function() x  #funct to get matrix x
  setinver <- function(solve) k <<- solve  
  getinver <- function() (k)   
  
  list(get = get,set = set,
       getinver = getinver,
       setinver = setinver)
}
#to get the cache data
cacheSolve <- function(x,...){    #to get cache data
  k <- x$getinver()
  if (!is.null(k)){               #to check if the inverse is null            
    message("getting cached data")
    return(k)
  }
  data <- x$get()
  k <- solve(data,...)
  x$setinver(k)
  k
}
