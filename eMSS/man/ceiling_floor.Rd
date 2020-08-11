\name{ceiling_floor}
\alias{ceiling_floor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Ceiling and Floor
}
\description{
Find the Suitable ceiling/floor,which are data driven.
ceiling < floor
}
\usage{
ceiling_floor(pvalue, lpvalue)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pvalue}{
  P-value file.
}
  \item{lpvalue}{
  length of p-value file.
}
}

\value{

  \item{suitable_ceiling}{
   the suitable ceiling value for this data. Ceiling value  will between 0 to 1}
  \item{suitable_floor}{
   the suitable floor value for this data. Floor value  will between 0 to 1 }

}

\examples{
 ceiling_floor(runif(100),100)
}
