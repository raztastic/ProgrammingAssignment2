
#makeCacheMatrix creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x=matrix()){
        m<-NULL
        set<-function(y){
                x<<- y
                m<<- NULL
        }
        get<- function() x
        setSolve <- function(solve) m<<-solve
        getSolve<-function() m
        list(set=set, get = get, setSolve=setSolve, getSolve=getSolve)
}


# computes inverse of special matrix returned by makeCacheMatrix
# if inverse has alredy been calculated (and matrix is the same)
# cacheSolve will retrieve the inverse from the cache

cacheSolve <-function(x,...){
        m<-x$getSolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-Solve(data,...)
        x$setSolve(m)
        m
}
