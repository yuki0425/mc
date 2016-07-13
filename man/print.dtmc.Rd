\name{print.dtmc, print.ctmc}
\alias{print.dtmc}
\alias{print.ctmc}

\title{
Show the Properties of the Markov Chain
}

\description{
Print the name, states, dimension and transition matrix of Markov Chain
}

\usage{
\method{print}{dtmc}(x, ...)
\method{print}{ctmc}(x, ...)
}

\arguments{
  \item{x}{
the Markov Chain we want to apply
}
  \item{\dots}{
Arguments to be passed to methods
}
}

\author{
Jiexuan Cao, Yuki Liu, Qianhan Zhang
}

\examples{

dtftmc = mc(states = c("sunny", "cloudy", "rain"),
                 pijdef = function(i,j,...) {
                           if (i == 1 && j < 3 ||
                               i == 2 && j != 2 ||
                               i == 3 && j > 1)  return(0.5)
                           0
                        },  
       name = "Weather")

dtftmc
dtftmc$states
dtftmc$pijdef

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

dtiftmc
dtiftmc$states
dtiftmc$pijdef

}