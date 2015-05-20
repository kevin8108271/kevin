
##這個程式碼是寫來計算逆矩陣，並在已計算過的情況下使用緩存，避免再次計算消耗時間
##This code is writed to solve the inverse matrix,and if the matrix has already calculated the inverse
##,it will find the answer from the cache,which can avoid waste time

##第一個部分makeCacheMatrix是一個函數用來特別創建一個list來保存此矩陣的相關資訊，以及緩存計算過的逆矩陣(若已計算)
##The first part makeCacheMatrix is a function using to create a list to preserve some attribute about 
##the matrix,and cache the inverse matrix if it has already been calculated
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
} 

##而第二個部分cacheSolve則在一開始會先確認輸入的值是否已經緩存有逆矩陣，若有則直接輸出，避免不必要的消耗時間，
##沒有的話才在進行計算，並將算出來的值緩存在第一個部分的list裡。
##The second part cacheSolve,it will check that your input is already cache the inverse matrix or not, to avoid
##spend time to do the same calculation ;and if not ,it will start calculate and input theinverse matrix to the
##first function's list to cache

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
