#The following two functions provide the means to store a matrix value in an object that caches that matrix's inverse
#The first, makeCacheMatrix(), takes as input a matrix value and outputs a list of functions by which that value and its inverse may be accessed
#The second, cachesolve(), takes as input a cacheMatrix object and outputs the inverse of the matrix held by that object

#this should create an object/closure to hold both a matrix and, once calculated, its inverse
makeCacheMatrix=function(m=matrix()){
    #the inverse of cacheMatrix m is initialized to NULL
    inv=NULL
    
    #this cacheMatrix function allows a symbol already pointing to a cacheMatrix object to acquire new values for m and inv
    set=function(n){
        #the m value of the parent environment is set to n
        m<<-n
        #the inv value of the parent environment is set to NULL
        inv<<-NULL
    }
    
    #this cacheMatrix function returns the value of cacheMatrix$m
    get=function(){m}
    
    #this cacheMatrix function calculates the inverse of cacheMatrix$m, and stores it in cacheMatrix$inv
    setInv=function(){
        #the inv value of the parent environment is set to the inversion of m
        inv<<-solve(m)
        inv
    }
    
    #this cacheMatrix function returns the value of cacheMatrix$inv
    getInv=function(){inv}
    
    list(set=set,get=get,setInv=setInv,getInv=getInv)
}

#this should return the inverse of a "cacheMatrix" object
cacheSolve=function(cm){
    inv = cm$getInv()
    
    #check if inverse is already cached
    if(!is.null(inv)){
        #if check passes, return inverse of cm$m from cache
        message("Inverse already cached:")
        inv
    }else{
        #otherwise call cm$setInv(), which calculates inverse, stores it to cache, and returns new inv value
        cm$setInv()
    }
}