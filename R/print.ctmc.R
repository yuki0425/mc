print.ctmc <-
function(x, ...) {
  # print the name, state, the dimension, the transition matrix and the holding-time rates 
  # of the current continuous Markov Chain
  cat("The name of the Markov Chain is", x$name, "\n")
  cat("A", x$dim, "- dimensional", x$timecategory ,
      "Markov Chain with following states \n")
  cat(x$states, "\n\nThe transition matrix (by rows) is defined as follows \n")
  print(x$pijdef)
  cat("\nThe holding-time rates is \n")
  print(x$qidef)
}
