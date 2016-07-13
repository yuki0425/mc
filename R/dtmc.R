dtmc <-
function(timecategory = 'discrete', states, pijdef, qidef = NULL, name, byrow = TRUE){
  dtMarkov = mc(timecategory, states, pijdef, qidef, name, byrow)
  
  class(dtMarkov) = append(class(dtMarkov), 'dtmc')
  return(dtMarkov)
}
