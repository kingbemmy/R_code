\name{excursion}
\alias{excursion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Excursion
}
\description{
Calculate the segmental score by Fisher's method, find the top three MSS regions.
}
\usage{
excursion(n, aa)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
length of p-values.
}
  \item{aa}{
Score of pvalues, which calculate by theta.MSS function.
}
}

\value{

  \item{stMSS}{
  the largest maximal segmental score after compute.}
  \item{ststartpos}{
  the first region start position.}
  \item{stmaxpos}{
  the mss value occur position.}
  \item{ndMSS}{
  the second largest maximal segmental score after compute.}
  \item{ndstartpos}{
  the second region start position.}
  \item{ndmaxpos}{
  the second mss value occur position.}
  \item{rdMSS}{
  the third largest maximal segmental score after compute.}
  \item{rdstartpos}{
  the third region start position.}
  \item{rdmaxpos}{
  the third mss value occur position.}
  \item{stnewin}{
  first region end position.}
  \item{ndnewin}{
  second region end position.}
  \item{rdnewin}{
  third region end position.}
}

\examples{
n=100
floor=0.2
excursion(n,theta.MSS(runif(n),floor))
}
