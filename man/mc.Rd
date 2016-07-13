\name{mc, ctmc, dtmc}
\alias{mc}
\alias{ctmc}
\alias{dtmc}

\title{
Create a Markov Chain Model
}
\description{
Create a Markov Chain Class.
}
\usage{
mc(timecategory = "discrete", states, pijdef, qidef = NULL, name, byrow = TRUE)

dtmc(timecategory = 'discrete', states, pijdef, qidef = NULL, name, byrow = TRUE)

ctmc(timecategory = 'continuous', states, pijdef, qidef, name, byrow = TRUE)

}

\arguments{
 \item{timecategory}{
indicate whether the Markov Chain is discrete time or continous time, default value is "discrete"
}
 \item{states}{
the states name of the Markov Chain 
}
 \item{pijdef}{
the transition matrix of the Markov Chain 
}
 \item{qidef}{
the holding-time rates for the continuous Markov Chain
}
 \item{name}{
the name of Markov Chain 
}
 \item{byrow}{
logical, indicate whether transition probabilities are shown by row or by column, default value is TURE
}
}

\details{
pijdef can either be in the matrix form or in the function form.
}

\author{
Jiexuan Cao, Yuki Liu, Qianhan Zhang
}

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
             
## continuous finite Markov Chain
ctftmc = mc(states = c("0", "1", "2"),
            pijdef = matrix(c(0, 0.2857143, 0, 1, 0, 1, 0, 0.7142857, 0), ncol = 3),
            qidef = c(0.25, 0.175, 0.08),
            name = 'Machine Repair')
}

