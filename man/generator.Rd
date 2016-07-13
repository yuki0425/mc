\name{generator}
\alias{generator}

\title{
infiniteseimal generator matrix
}

\description{
infiniteseimal generator matrix for finite state continuous markov chain
}

\usage{
generator(ctmc, bycol = TRUE)
}

\arguments{
  \item{ctmc}{
a ctmc class object
}
  \item{bycol}{
logical, indicate whether the generater matrix columns sum to zero, default value is TRUE
}
}

\value{
A matrix corresponding to the infiniteseimal generator matrix
}

\author{
Jiexuan Cao, Yuki Liu, Qianhan Zhang
}

\examples{
a = mc(states = c("high", "low"),
       pijdef = matrix(data = c(0,1,1,0), nrow = 2),
       qidef = c(3,1),
       name = 'temperature')

generator(a)

}
