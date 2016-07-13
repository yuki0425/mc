mc <-
function(timecategory = 'discrete', states, pijdef, qidef = NULL, name, byrow = TRUE){
  # create an instance of the "mc" class in order to find stationary distribution for later use
  # two choice for input state: "infinity" or user defined state name
  
  if(states[1] != "infinity" & is.function(pijdef)){
    #if the user input 'pijdef' is a function and it's the finite state case
    #change pijdef from function to the matrix
    n = length(states)
    transition_matrix = matrix(rep(0, n*n), nrow = n)
    for (i in 1:n){
      for (j in 1:n){
        transition_matrix[i,j] = pijdef(i,j)
      }
    }    
  } else
    transition_matrix = pijdef
  # for the continuous markov chain
  if(! is.null(qidef)){ 
    timecategory = 'continuous'
    if(states[1] != "infinity" & is.function(qidef)){
      #if the user input 'qidef' is a function and it's the finite state case
      #change qidef from function to the vector
      n = length(states)
      holdingtime_rate = rep(0,n)
      for (i in 1:n){
        holdingtime_rate[i] = qidef(i)
      }    
    } else
      holdingtime_rate = qidef
  }
  
  
  if (timecategory == "discrete"){
    #for the finite situation the transition matrix is in the matrix form
    if (states[1] != "infinity"){
      rownames(transition_matrix) = states
      colnames(transition_matrix) = states
      Markov = list(
        name = name,
        states = states,
        pijdef = transition_matrix,
        dim = dim(transition_matrix)[1],
        timecategory = "discrete",
        byrow = byrow
      )
    } else{#for the infinite situation the transition matrix is in the function form
      Markov = list(
        name = name,
        states = "infinity",
        pijdef = pijdef,
        dim = "infinity",
        timecategory = "discrete",
        byrow = byrow
      )
    }
  }
  
  
  if (timecategory == "continuous"){
    #for the finite situation the transition matrix is in the matrix form, holding-time rates is in the vector form
    if (states[1] != "infinity"){
      rownames(transition_matrix) = states
      colnames(transition_matrix) = states
      names(holdingtime_rate) = states
      Markov = list(
        name = name,
        states = states,
        qidef = holdingtime_rate,
        pijdef = transition_matrix,
        dim = dim(transition_matrix)[1],
        timecategory = "continuous",
        byrow = byrow
      )
    } else{#for the infinite situation the transition matrix and holding-time rates are in the function form
      Markov = list(
        name = name,
        states = "infinity",
        pijdef = pijdef,
        qidef = qidef,
        dim = "infinity",
        timecategory = "continuous",
        byrow = byrow
      )
    }
  }
  
  
  ## Set the name for the class
  if(Markov$timecategory == "discrete")
    class(Markov) = c('mc', 'dtmc')
  else
    class(Markov) = c('mc', 'ctmc')
  return(Markov)
}
