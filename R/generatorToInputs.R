generatorToInputs <-
function(generator, bycol = TRUE){
  # get the inputs 'qidef' & 'pijdef' from a generator
  # Note that the generator matrix here is by column by default
  ## generator: infinitesimal generator matrix
  ## bycol: Flag to determine if generator's columns(row) sum to 0, columns sum to 0 by default
  
  if (bycol == TRUE) generator = t(generator)
  pijdef = generatorToTransitionMatrix(generator)
  qidef = - diag(generator)
  return(list(pijdef = pijdef, qidef = qidef))
}
