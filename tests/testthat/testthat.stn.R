test.stn = function(){
  p = matrix(rep(0,9), nrow = 3)
  p[1,1] <- 0.5
  p[1,2] <- 0.5
  p[2,3] <- 0.5
  p[2,1] <- 0.5
  p[3,1] <- 1
  n = nrow(p)
  imp = diag(n) - t(p)
  imp[n,] = rep(1,n)
  rhs = c(rep(0,n-1),1)
  stationary = solve (imp , rhs) 
  test1= mc(states = c("1", "2", "3"),
            pijdef = p,  name = "3-heads-in-a-row")
  
  checkEquals(stn(test1), stationary)
  # the example is obtained from the following website:
  #"http://www.sciencedirect.com/science/article/pii/S0024379508000244"
  # page 2738 Example2
  
  test2 = mc(states = as.character(seq(1, 10, 1)),
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
  # using example formula to obtain vector of pi (matrix form)
  pi = NULL
  for (j in 1:n){
      if (j==n) {
          pi[j] = 2^(-(n-1))
      }
      if (j>=1 && j<=n-1) {
          pi[j] = 2^(-j)
      }
  }
  
  checkEquals(round(stn(test2), 10), round(matrix(pi, 1, 10),10))
  
  ## machine repair for continous finite markov chain
  test3 = mc(states = c("0", "1", "2"),
              pijdef = matrix(c(0, 0.2857143, 0, 1, 0, 1, 0, 0.7142857, 0), ncol = 3),
              qidef = c(0.25, 0.175, 0.08),
              name = 'Machine Repair')
  
  checkEquals(round(stn(test3), digits=2), c(0.072, 0.362, 0.566))
}