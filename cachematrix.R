#first create matrix and a inverse matrix (to serve as control)
q <- matrix(replicate(20,rnorm(20)),nrow=20,ncol=20)
r <- solve(q)

#first function to cache an inverse list, but it returns a non-inverted matrix (the original)
makeCachematrix <- function(x = matrix())
  {
  m <- NULL
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  z <<- list(get=get, setsolve=setsolve, getsolve=getsolve)
  return(x)
  }
#the result of fuction is thus the original matrix, but cache is inverted
s <- makeCachematrix(q)


cacheSolve <- function(x, ...) 
  {
  if(exists("z"))
    {
    x <- z
    m <- x$getsolve()
    }
  if(!is.null(m)) 
    {
    message("getting cached data")
    x <- m
    return(m)
    }
  else
    {
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    x <- m
    return(x)
    }
  }

t <- cacheSolve(s)
u <- cacheSolve(q)

#check function
identical(t,u)
identical(r,u)

#remove
rm(list=ls())

