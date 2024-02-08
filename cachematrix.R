#by the adjoint method
#A^-1=([A*]^t/|A|)

makeCacheMatrix<-function(x){
      n<-nrow(x)
      adj<-matrix(NA,n,n)
      minor<-function(matrix,i,j){
            det(matrix[-i,-j])
      }
      cofactor<-function(matrix,i,j){
            (-1)^(i+j)*minor(matrix,i,j)
      }
      for(i in 1:n)
            for(j in 1:n)
                  adj[i,j]<-cofactor(x,i,j)
      inverse<-t(adj)/det(x)
      inverse
}

cacheSolve<-function(x){
      inverse<-makeCacheMatrix(x)
      sol_inverse<-solve(inverse)
      if (identical(sol_inverse,x)=="TRUE")
            sol_inverse<-makeCacheMatrix(x)
      sol_inverse
}

