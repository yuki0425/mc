\name{mc-package}
\alias{mc-package}
\docType{package}
\title{
  Markov Chain Model
}
\description{
  This package contains several functions to handle and analysis both discrete and continous time Markov Chain Model with infinite and finite states.
}

\author{
Jiexuan Cao,Yuki Liu, Qianhan Zhang

Maintainer:
Jiexuan Cao <jxucao@ucdavis.edu>
}
\references{
Matloff, Norm. "Chapter 6: Introduction to Discrete Markov Chains." From Algorithms to Z-Scores: Probabilistic and Statistical Modeling in Computer Science. Davis: U of California, 2014. 109-23. Print.
}

\keyword{ package }

\examples{

## discrete finite Markov Chain
dtftmc = mc(states = c("sunny", "cloudy", "rain"),
                 pijdef = function(i,j,...) {
                           if (i == 1 && j < 3 ||
                               i == 2 && j != 2 ||
                               i == 3 && j > 1)  return(0.5)
                           0
                        },
       name = "Weather")


## discrete infinite Markov Chain
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

## get states of the Markov Chain
dtftmc$states
dtiftmc$states

## get transition matrix of the Markov Chain
dtftmc$pijdef
dtiftmc$pijdef

##get stationary distribution of the Markov Chain function
dtftsd = stn(dtftmc)
dtiftsd = stn(dtiftmc)

##get submatrix of expected hitting times function, with rows and columns specified by the user as i j.
dtftht = hittingtime(dtftmc, c(1,2), c(2,3))
dtiftht = hittingtime(dtiftmc, c(1,2), c(2,3))

}
