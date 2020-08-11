\name{eMSS}
\alias{eMSS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
expanded maximal segmental score (eMSS)
}
\description{
eMSS analysis.
Return Ceiling,Floor,Step1~Step3 output(top three MSS),draw MSS graphics
The first picture is the original plot, the second to the fourth picture is the first refinement, the fifth to the seventh picture is the last refinement.
}
\usage{
eMSS(p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
P-value file. The p-value file should be a matrix.
}
}

\value{
  \item{Ceiling}{
  Ceiling value.}
  \item{Floor}{
  Original Floor value.}
  \item{original output}{
  Step=1 output.}
  \item{first refinement}{
  1st MSS region first refinement.}
  \item{second refinement}{
  2nd MSS region first refinement.}
  \item{third refinement}{
  3rd MSS region first refinement.}
  \item{first final}{
  1st MSS region final refinement.}
  \item{second final}{
  2nd MSS region final refinement.}
  \item{third final}{
  3rd MSS region final refinement.}
}
\references{
  Hsieh, A. R., Sie, J. J., Chang, C. C., Ott, J., Lian, I. B., & Fann, C. (2020).
  Maximal Segmental Score Method for Localizing Recessive Disease Variants Based on Sequence Data.
  \emph{Frontiers in genetics, 11}, 555.
  \doi{10.3389/fgene.2020.00555}
}
\author{
Jia-Jyun Sie
}
\examples{

  pvalue<-matrix(runif(1000),1000,1)
  eMSS(pvalue)

}
