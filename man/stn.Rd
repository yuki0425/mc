\name{stn}
\alias{stn}

\title{
Generic function for the stn function
}

\description{
Generic function for the stn function, function for S3 Class use
}

\usage{
stn(obj, k, ending, epsilon)
}

\arguments{
  \item{obj}{
the Markov Chain we want to apply
}
  \item{k}{
for infinite state Markov Chain, the initial state to do the iterative process in order to find the stationary distribution
}
  \item{ending}{
for infinite state Markov Chain, the ending criterion of the iterative process
}
  \item{epsilon}{
for infinite state Markov Chain, the convergence criterion to stop the iterative process
}
}

\value{
A matrix corresponding to the stationary states
}

\author{
Jiexuan Cao, Yuki Liu, Qianhan Zhang
}
