## cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix", which contains a list of function to 
## 1. set the matrix
## 2. get the matrix,
## 3. set the inversion of the matrix,
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        # initialize inverse matrix(s) to NULL is needed. When cacheSolve is called 
        # immediately after makeCacheMatrix is constructed, the inverse can be 
        # computed in cacheSolve()
        s<-NULL
        # function to set new value to the underline matrix
        set<-function(y) {
                # set the new matrix to x (reset x). as x is not defined in the 
                # local set(), <<- is used
                x<<-y
                # reset inverse matrix to NULL is needed. As the matrix has changes, 
                # the cache need to be empty. <<- is used as s is not defined in
                # the local set()
                s<<-NULL
        }
        # function to get the matrix. This is needed for cacheSolve() to compute
        # the inverse matrix when cache is empty
        getmatrix<-function() x
        # function to set the inverse matrix to s, which is computed by cacheSolve()
        # <<- is used as s is not defined in the local setinverse()
        setinverse<-function(solve){
                s<<-solve
        }
        # function to get the inverse matrix. This is first called by cacheSolve()
        # to check whether the inverse is cached already. It will be NULL if 
        # setinverse() has not been called, or immediately after set() is called
        getinverse<-function() s
        # return value of makeCacheMatrix as a list of functions which can be 
        # accessed using $
        list(set=set, getmatrix=getmatrix, setinverse=setinverse, getinverse=getinverse)


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## and save it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # get the inverse matrix from x. If setinverse() has not been called or
        # the cache has not been emptied by set(), it will be NULL. If inverse matrix
        # has been stored in cache, it will not be NULL.
        s<-x$getinverse()
        if(!is.null(s)) {
                # if s is not NULL, show the message, then return the cached inverse
                # matrix
                message("get cached data")
                return(s)
        }
        # if s is NULL, get the matrix from x
        data<-x$getmatrix()
        # compute the inversion
        s<-solve(data, ...)
        # store the inverse matrix to cache
        x$setinverse(s)
        # return the inverse matrix
        s
}
