\name{theta.MSS}
\alias{theta.MSS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Theta.MSS
}
\description{
Convert p-value to score.
}
\usage{
theta.MSS(datapvalue, Floor)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datapvalue}{
P-value file. Should be 0~1.
}
  \item{Floor}{
Floor value.
}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
theta.MSS(runif(100),0.2)
}
