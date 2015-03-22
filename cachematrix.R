#-------------------------------------------------------------------------------
# cachematrix.R
# This file implements a Cached Matrix solution
#
# Usage:
# cacheSolve(makeCacheMatrix(matrix(1:4,2,2)))   # solve and return
# cacheSolve(makeCacheMatrix(matrix(1:4,2,2)))   # retrieve from cache
# cacheSolve(makeCacheMatrix(matrix(10:13,2,2))) # solve and return
#
# Note:
# Aplogies for C++ and Perl like constructs and concepts (old habits die hard)
#
# nram, 03/22/2015

#-------------------------------------------------------------------------------
# Class CacheMatrix
# 
CacheMatrix <- function(m = matrix)
{
   
   me <- list (
      m_matrix = m,
      m_key = paste(m,collapse="-"),
      #m_key = paste0(m),
      m_invmatrix = NULL
   )
   
   ## Set the name for the class
   class(me) <- append(class(me),"CacheMatrix")
   return(me)
}

#-------------------------------------------------------------------------------
# S3 member functions 
m_solve <- function(o)
{
   # Calling the base m_solve function")
   UseMethod("m_solve",o)
}

m_solve.default <- function(o)
{
   # what now?
   return(o)
}

#-------------------------------------------------------------------------------
# m_solve()
# member function to solve matrix
# solve matrix, store in gloabl cache and return object
m_solve.CacheMatrix <- function(o)
{
   # handle this class's implementation
   o$m_invmatrix <- solve (o$m_matrix)
   #print ("inversion matrix"); print(o$m_invmatrix)
   #print ("Store inverse at key: "); print(o$m_key)
   g_cache[[o$m_key]] <<- o$m_invmatrix 
   return(o)
}

#-------------------------------------------------------------------------------
# m_is_solved()
# member function to check if the matrix is already solved
# returns TRUE/FALSE 
m_is_solved <- function(o) { UseMethod("m_is_solved",o) }
m_is_solved.default <- function(o) {  return(o) }
m_is_solved.CacheMatrix <- function(o) {
   if (o$m_key %in% names(g_cache)) {
      return(TRUE)
   }
   FALSE
}

#-------------------------------------------------------------------------------
# makeCacheMatrix
# This just creates CacheMatrix object and returns
makeCacheMatrix <- function(x = matrix()) {
   #env <- .GlobalEnv
   o <- CacheMatrix(x)
   o
}

#-------------------------------------------------------------------------------
# cacheSolve
# use CacheMatrix member functions to either return already solved matrix
# or solve now, store and return result
cacheSolve <- function (x, ... ) {
   if (!m_is_solved(x)) {
      m_solve(x)
   } else {
      print ("Already solved. Get from cache")
   }
   g_cache[[x$m_key]]
}