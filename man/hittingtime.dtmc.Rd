\name{hittingtime.dtmc, hittingtime.ctmc}
\alias{hittingtime.dtmc}
\alias{hittingtime.ctmc}

\title{
Calculate the Submatrix of Expected Hitting Times
}

\description{
Calculate the submatrix of expected hitting times, with rows and columns specified by the user.
}

\usage{
\method{hittingtime}{dtmc}(obj, i, j, k = 10, epsilon = 0.05)
\method{hittingtime}{ctmc}(obj, i, j, k = 10, epsilon = 0.05)
}

\arguments{
  \item{obj}{
the Markov Chain we want to apply
}
  \item{i}{
user defined rows of the submatrix
}
  \item{j}{
user defined columns of the submatrix
}
  \item{k}{
for infinite state Markov Chain, the initial state to do the iterative process in order to find the stationary distribution, default value is 10
}
  \item{epsilon}{
for infinite state Markov Chain, the convergence criterion to stop the iterative process, default value is 0.05
}
}

\details{
pijdef can either be in the matrix form or in the function form.
}

\value{
A matrix corresponding to the expected hitting times
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

dtftht = hittingtime(dtftmc, c(1,2), c(1,3))


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

dtiftht = hittingtime(dtiftmc, c(1,2), c(1,3))

ctftmc = mc(states = c("0", "1", "2"),
            pijdef = matrix(c(0, 0.2857143, 0, 1, 0, 1, 0, 0.7142857, 0), ncol = 3),
            qidef = c(0.25, 0.175, 0.08),
            name = 'Machine Repair')
            
hittingtime(ctftmc, c(1,2), c(2,3))

}