generator <-
function(ctmc, bycol = TRUE){
  # get the infiniteseimal generator matrix for finite state continuous markov chain
  # Note that the infiniteseimal generator matrix here is by column by default
  # the transpose of the generator in package 'markovchain' is by row by default
  if (! is.matrix(ctmc$pijdef)){
    stop("The markov chain should be finite state space markov chian")
  }
  # if it is the finite state case
  n = nrow(ctmc$pijdef)
  Q = matrix(0, n, n)
  for (i in 1:n){
    for (j in 1:n){
      if (i == j) Q[i,j] = -ctmc$qidef[i]
      else Q[i,j] = ctmc$qidef[j] * ctmc$pijdef[j,i]
    }
  }
  dimnames(Q) = list(ctmc$states, ctmc$states)
  if (bycol == FALSE)
    Q = t(Q)
  
  return(Q)
}
