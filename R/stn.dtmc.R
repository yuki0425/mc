stn.dtmc <-
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
    
    DTMC = new("markovchain", states = as.character(obj$states), 
               byrow = obj$byrow, transitionMatrix = obj$pijdef, name = obj$name)
    
    steadystate = steadyStates(DTMC)
    
  } else{
    
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
      #use "markovchain" package to get the markov chain of different k
      DTMC = new("markovchain", states = as.character(seq(1,k)), 
                 byrow = obj$byrow, transitionMatrix = pijdef, name = obj$name)
      #ss is the stationary distribution of current markov chain
      #swap 
      ss = steadyStates(DTMC)
      steadystate[[1]] = steadystate[[2]]
      steadystate[[2]] = ss
      
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
