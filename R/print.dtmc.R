print.dtmc <-
function(x, ...) {
  # print the name, state, the dimension, and the transition matrix of the current discrete Markov Chain
  cat("The name of the Markov Chain is", x$name, "\n")
  cat("A", x$dim, "- dimensional", x$timecategory ,
      "Markov Chain with following states \n")
  cat(x$states, "\n\nThe transition matrix (by rows) is defined as follows \n")
  print(x$pijdef)
}
