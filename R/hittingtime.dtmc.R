hittingtime.dtmc <-
function(obj, i, j, k = 10, epsilon = 0.05){
  # k: initial state
  # episolon: the convergence criterion to stop the iterative process, default value is 0.05
  
  # function: calculate the hitting time
  findht = function(p, i, j) { #i can be a vector, j is an integer
    transitionmatrix = p
    p = p[-j,-j]
    n = nrow(p)
    q = diag(n) - p
    ones = rep(1, n)
    result = solve(q, ones) 
    result = append(result, 1 + transitionmatrix[j,-j]%*%result,after = j-1)
    return(result[i])  #return i(vector) to j vector
  }
  
  if (is.matrix(obj$pijdef)){
    #get the submatrix
    submatrix = matrix(rep(0, length(i)*length(j)), nrow = length(i))
    
    ii = 1
    for(jj in j){
      submatrix[,ii] = unname(findht(obj$pijdef, i, jj))
      ii = ii + 1
    }
  } 
  else{
    # if there is no stationary distribution
    # does not consider hitting time
    if (stn(obj)[1] == "No stationary distribution exists!"){
      submatrix = "No hitting time exists!"
    } else{
      tmp = 0 #count for the number of consecutive interval of steadystate less than the epsilon
      
      #submatrix list to compare find the difference value less than epsilon
      submatrix_list = list(matrix(rep(0, length(i)*length(j)), nrow = length(i)), 
                            matrix(rep(0, length(i)*length(j)), nrow = length(i)))
      
      while (tmp < 4){
        pijdef = matrix(rep(0, k*k), nrow = k)
        for (ii in 1:k){
          for( jj in 1:k){
            pijdef[ii,jj] = obj$pijdef(ii,jj)
          }
        }
        
        for (ii in 1:k){
          summation = 0
          for (jj in 1:k){
            summation = summation + pijdef[ii,jj]
          }
          if (summation != 1){
            pijdef[ii,] = pijdef[ii,]/summation
          }
        }
        
        #get the submatrix
        submatrix = matrix(rep(0, length(i)*length(j)), nrow = length(i))
        
        ii = 1
        for(jj in j){
          submatrix[,ii] = unname(findht(pijdef, i, jj))
          ii = ii + 1
        }
        
        submatrix_list[[1]] = submatrix_list[[2]]
        submatrix_list[[2]] = submatrix
        
        if (all(abs(submatrix_list[[1]] - submatrix_list[[2]]) < epsilon)){
          tmp = tmp + 1
        } else
          tmp = 0
        k = k + k  
      }#while end
    }# else end
  }#else end
  
  rownames(submatrix) = as.character(i)
  colnames(submatrix) = as.character(j)
  
  return(submatrix)
}
