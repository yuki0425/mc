stn.ctmc <-
function(obj, k = 10, ending = 10, epsilon = 0.05){
  # 'k': the initial state to do the iterative process in order to find the stationary distribution,
  # default value of k is 10
  # 'ending': number of iteration times
  # 'epsilon': the convergence criterion to stop the iterative process, default value is 0.05
  # find the stationary distribution
  # ouput the pi (as a vector)
  kk = k
  # increment k at a time
  
  if (is.matrix(obj$pijdef)){ #if it is the finite state case
    
    Q = generator(obj, bycol = FALSE)
    CTMC = new('ctmc', states = as.character(obj$states), byrow = TRUE, generator = Q, name = obj$name) 
    
    steadystate = steadyStates(CTMC)
    
  } 
  else {  # if it is the infinite state case
    
    tmp = 0 #count for the number of consecutive interval of steadystate less than the epsilon
    steadystate = list(rep(0, k), rep(0, k))
    end = 0
    while (tmp < 4 & end < ending){
      #calculate the transition matrix
      pijdef = matrix(rep(0, k*k), nrow = k)
      for (i in 1:k){
        for( j in 1:k){
          pijdef[i,j] = obj$pijdef(i,j)
        }
      }
      
      for (i in 1:k){
        summation = 0
        for (j in 1:k){
          summation = summation + pijdef[i,j]
        }
        if (summation != 1){
          pijdef[i,] = pijdef[i,]/summation
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
      
      #use "markovchain" packge to get the markov chain of different k
      CTMC = new("ctmc", states = as.character(seq(1,k)), 
                 byrow = TRUE, generator = Q, name = obj$name)

      #swap 
      steadystate[[1]] = steadystate[[2]]
      steadystate[[2]] = steadyStates(CTMC)
      
      if (all(abs(as.vector(steadystate[[1]]) - as.vector(steadystate[[2]][-c(k - kk + 1:k)])) < epsilon)){
        tmp = tmp + 1
      } else
        tmp = 0
      k = k + kk  
      
      end = end + 1
    }
    
    if (end == ending){
      steadystate = "No stationary distribution exists!"
    } else{
      steadystate = steadystate[[2]]
    }
  }
  
  return(steadystate)
}
