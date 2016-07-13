\name{generatorToInputs}
\alias{generatorToInputs}

\title{
'qidef' & 'pijdef' from a generator matrix
}

\description{
Get the inputs 'qidef' & 'pijdef' from a infiniteseimal generator matrix
}

\usage{
generatorToInputs(generator, bycol = TRUE)
}

\arguments{
  \item{generator}{
infiniteseimal generator matrix
}
  \item{bycol}{
logical, indicate whether the generater matrix columns sum to zero, default value is TRUE
}
}

\value{
 \item{pijdef}{the transition matrix of Markov Chain, shown by row}
 \item{qidef}{the holding-time rates for the continuous Markov Chain}
}

\author{
Jiexuan Cao, Yuki Liu, Qianhan Zhang
}

\examples{
q = 0.5
alpha = 0.4
sigma = 0.3
mu = 0.2
omega = 0.1

Q =
  rbind(c(-alpha, sigma, 0, mu, 0, 0),
        c(alpha*(1 - q), -(alpha + sigma), (omega + sigma), 0, mu, 0),
        c(0, alpha, -(sigma + omega), 0, 0, 0),
        c(alpha*q, 0, 0, -(alpha + mu), sigma, (mu + omega)),
        c(0, 0, 0, alpha*(1-q), -(sigma + mu), 0),
        c(0, 0, 0, alpha*q, 0, -(mu + omega)))

generatorToInputs(Q)
}
