hittingtime.ctmc <-
function(obj, i, j, k = 10, epsilon = 0.05){
  # k: initial state
  # episolon: the convergence criterion to stop the iterative process, default value is 0.05
  
  # function: calculate the hitting time
  
#   findht = function(p, i, j) { #i can be a vector, j is an integer
#     transitionmatrix = p
#     p = p[-j,-j]
#     n = nrow(p)
#     q = diag(n) - p
#     ones = rep(1, n)
#     result = solve(q, ones) 
#     result = append(result, 1 + transitionmatrix[j,-j]%*%result,after = j-1)
#     return(result[i])  #return i(vector) to j vector
#   }
  
  findht = function(Q, i, j) { #i can be a vector, j is an integer
    q = Q[-j,-j]
    n = nrow(q)
    minus_ones = rep(-1, n)
    result = solve(q, minus_ones)
    result = append(result, 0, after = j-1)
    return(result[i])
  }
  
  if (is.matrix(obj$pijdef)){ #finite state 
    Q = generator(obj, bycol = FALSE) # row sum to 0
    
    #get the submatrix
    submatrix = matrix(0, nrow = length(i), ncol = length(j))
    
    ii = 1
    for(jj in j){
      submatrix[,ii] = unname(findht(Q, i, jj))
      ii = ii + 1
    }
  }
  else { #infinite state
    # if there is no stationary distribution
    # does not consider hitting time
    if (stn(obj)[1] == "No stationary distribution exists!"){
      submatrix = "No hitting time exists!"
    } 
    else {
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
        
        #calculate the holding-time rate
        qi = rep(0, k)
        for (i in 1:k){
          qi[i] = obj$qidef(i)
        }  
        
        #get the generator matrix
        Q = matrix(0, k, k)
        for (i in 1:k){
          for (j in 1:k){
            if (i == j) Q[i,j] = -qi[i]
            else Q[i,j] = qi[j] * pijdef[j,i]
          }
        }
        Q = t(Q)
        
        for (i in 1:k){
            summation = 0
            for (j in 1:k){
                summation = summation + Q[i,j]
            }
            if (summation != 0){
                Q[i,] = Q[i,]/summation
            }
        }
        
        #get the submatrix
        submatrix = matrix(0, nrow = length(i), ncol = length(j))
        
        ii = 1
        for(jj in j){
          submatrix[,ii] = unname(findht(Q, i, jj))
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
