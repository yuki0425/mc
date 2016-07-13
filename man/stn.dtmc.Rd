\name{stn.dtmc, stn.ctmc}
\alias{stn.dtmc}
\alias{stn.ctmc}

\title{
Calculate the Stationary Distribution of Markov Chain
}
\description{
Calculate the Stationary Distribution of Markov Chain
}
\usage{
\method{stn}{dtmc}(obj, k = 10, ending = 10, epsilon = 0.05)
\method{stn}{ctmc}(obj, k = 10, ending = 10, epsilon = 0.05)
}

\arguments{
  \item{obj}{
the Markov Chain we want to apply
}
  \item{k}{
for infinite state Markov Chain, the initial state to do the iterative process in order to find the stationary distribution, default value is 10
}
  \item{ending}{
for infinite state Markov Chain, the ending criterion of the iterative process, default value is 10
}
  \item{epsilon}{
for infinite state Markov Chain, the convergence criterion to stop the iterative process, default value is 0.05
}
}

\value{
A matrix corresponding to the stationary states
}

\author{
Jiexuan Cao, Yuki Liu, Qianhan Zhang
}

\examples{
#discrete finite state markov chain
dtftmc = mc(states = c("sunny", "cloudy", "rain"),
                 pijdef = function(i,j,...) {
                           if (i == 1 && j < 3 ||
                               i == 2 && j != 2 ||
                               i == 3 && j > 1)  return(0.5)
                           0
                        },  
       name = "Weather")

dtftsd = stn(dtftmc)

pijdeffunction = function(i,j) {
    p = 0.3
    q = 0.7
    r = 0
    if (i == 1 && j == 1)
        return (1 - p)
    if(j == i + 1)
        return(p)
    if(j == i - 1)
        return(q)
    if(j==i)
        return(r)
    return(0)
}

dtiftmc = mc(states = "infinity",
             pijdef = pijdeffunction, 
             name = "death and birth")

dtiftsd = stn(dtiftmc, ending = 20)

#continuous finite state markov chain
ctftmc = mc(states = c("0", "1", "2"),
            pijdef = matrix(c(0, 0.2857143, 0, 1, 0, 1, 0, 0.7142857, 0), ncol = 3),
            qidef = c(0.25, 0.175, 0.08),
            name = 'Machine Repair')
            
stn(ctftmc)

}