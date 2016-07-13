test.hittingtime = function() {
  # the example is obtained from the following website:
  #"http://www.sciencedirect.com/science/article/pii/S0024379508000244"
  # page 2738 Example2
  test1 = mc(states = as.character(seq(1, 10, 1)), 
             pijdef = function(i, j, n= 10) {
               if (j == 1 && i>=1 && i<=n)
                 return (0.5)
               if (j == i + 1)
                 return (0.5)
               if (j == i && j==n) 
                 return (0.5)
               else
                 return (0)  
             }, 
             name = "winning streak" )
  # check hitting time from 1 to 1, 1 to 2, .... all the way until 6 to 8
  i = seq(1,6,1)
  j = seq(1,8,1)
  hitting = matrix(rep(0, length(i)*length(j)), length(i), length(j))
  for (a in 1:length(i)) {
    for (b in 1:length(j)) {
      if (a<b) {
        hitting[a,b] = 2^b-2^a
      }
      else hitting[a,b] = 2^b
    }
  }
  checkEquals(hittingtime(test1, i, j), hitting)
}