test.infinity = function(){
    #the example is obtained from the following website:
    #http://www.rle.mit.edu/rgallager/documents/6.262-6vaw.pdf
    
    pijdeffunction = function(i,j){
        p = 0.3
        q = 0.7
        r = 0
        if (i == 1 && j == 1)
            return (1 - p)
        if(j == i+1)
            return(p)
        if(j == i-1)
            return(q)
        if(j==i)
            return(r)
        return(0)
    }
    
    
    test1= mc(states = "infinity",
            pijdef = pijdeffunction,  name = "death and birth model")
    st = stn(test1)
    
    n = length(st)
    rho = 0.3/0.7
    
    pi = rep(0, n)
    pi[1] = rho/(1 - rho)
    for (i in 2:n){
        pi[i] = pi[i-1] *rho
    }
    
    st = round(st, 6)
    pi = round(pi, 6)
    
    checkEquals(st, pi)
  
  
}