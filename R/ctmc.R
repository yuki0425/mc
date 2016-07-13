ctmc <-
function(timecategory = 'continuous', states, pijdef, qidef, name, byrow = TRUE){
  ctMarkov = mc(timecategory, states, pijdef, qidef, name, byrow)
  
  class(ctMarkov) = append(class(ctMarkov), 'ctmc')
  return(ctMarkov)
}
