@@ -1,15 +1,60 @@
## Put comments here that give an overall description of what your
## functions do
##Cache means that every time we need a result, we first check whether or not 
##we have calculated it before.
##If so, we could use it directly;
##or else, we need to work it out and save it in the cache for future use.

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse.
##There are two variables in this function, namely x and m
##where x is the matrix needing to be processed 
##and m is to store the results having been processed.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function(x) x
        setinverse<-function(inverse) m<<-solve(inverse)
        getinverse<-function() m
        list(set=set,
             get=get,
             setinverse=setinverse,
             getinverse=getinverse)
        ##The makeCacheMatrix returns a list whose elements are the
        ##functions defined before.

}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed)
##then the cachesolve should retrieve the inverse from the cache.
##That is, whenever we want to get the inverse of a certain matrix, 
##we first refer to the cache to find out whether or not 
##we have calculated and saved it before. 
##If we could find it in the cache, which means that the is.null is FALSE, 
##we can save the effort to do the calculation once more rather than 
##using the result in the cache;
##If not, we have to calculate it ourselves and save it to the cache after 
##calculating it for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinverse(x)
        ##That is to invoke the "getinverse" function in makeCacheMatrix.
        if(!is.null(m)){
                message("getting cached data")
                return(m)
                ##If m is not NULL, then we must have calculated 
                ##the inverse matrix before,
                ##so we could return the result available.
        }
        
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
}
