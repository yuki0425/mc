\name{hittingtime}
\alias{hittingtime}

\title{
Generic function for the hittingtime function
}

\description{
Generic function for the hittingtime function, function for S3 Class use
}

\usage{
hittingtime(obj, i, j, k, epsilon)
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
for infinite state Markov Chain, the initial state to do the iterative process in order to find the stationary distribution
}
  \item{epsilon}{
for infinite state Markov Chain, the convergence criterion to stop the iterative process
}
}

\author{
Jiexuan Cao, Yuki Liu, Qianhan Zhang
}
