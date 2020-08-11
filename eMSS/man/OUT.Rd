\name{OUT}
\alias{OUT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
OUT
}
\description{
This function calculates the value and draws MSS graphs at each stage and lists the floor value used.
}
\usage{
OUT(pvalue, Ceiling, Floor, Step, gene, beginning = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pvalue}{
P-value file.
}
  \item{Ceiling}{
Ceiling value. Calculated by ceiling_floor function.
}
  \item{Floor}{
Floor value. Calculated by ceiling_floor function.
}
  \item{Step}{
What stage is now. Step=1,2 or 3
}
  \item{gene}{
gene file. Length of gene file should be same as pvalue file. Default is NA, unless a gene file is given.
}
  \item{beginning}{
Reanalysis beginning position, will be used in reanalysis. Default is NA.
}
}

\value{
  \item{Output}{
  MSS calculation result.}
    \item{floor}{
  The floor value used in this calculation phase. When Step=1,Floor will not be printed.}


}

\examples{
  OUT(runif(100),0.1,0.3,1,matrix(1:100,100,1))
}
