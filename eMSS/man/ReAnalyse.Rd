\name{ReAnalyse}
\alias{ReAnalyse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ReAnalyse
}
\description{
This function will take out the top three MSS intervals and re-analyze them using stricter conditions and the OUT function.
This function cannot operate independently, it must be linked with the OUT function.
}
\usage{
ReAnalyse(output, P, re = NA, order, Ceiling, Floor, Step, gene, lgene = lgene)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{output}{
OUT function output
}
  \item{P}{
P-value file.
}
  \item{re}{
reanalysis from redo_first_1/redo_second_1/redo_third_1, default is NA. re=NA means first refinement.
}
  \item{order}{
which order of MSS, order=1 means 1st MSS; order=2 means 2nd MSS; order=3 means 3rd MSS.
}
  \item{Ceiling}{
Ceiling value.
}
  \item{Floor}{
Floor value.
}
  \item{Step}{
which step is it. In ReAnalyse function, step=2, 3.
}
  \item{gene}{
gene file.
}
  \item{lgene}{
Length of gene file.
}
}
\value{
This function linked with the OUT function.
The returned value is new floor and new output.
The ReAnalyse function uses stricter conditions to analyze MSS, so the floor will change.
    \item{floor}{
  The new floor value used.}
    \item{Output}{
  MSS calculation result.}

}

\note{
This function cannot operate independently, it linked with the OUT function.
}
