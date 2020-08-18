\name{MSS}
\alias{MSS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Maximal Segmental Score
}
\description{
Compute maximal segmental score(MSS) and graph
}
\usage{
MSS(permu, Floor, Step = 1, beginning = NA, graph = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{permu}{
P-value file.
}
  \item{Floor}{
Floor value.
}
  \item{Step}{
What is the Step now. Step should be 1,2 or 3
}
  \item{beginning}{
Reanalysis beginning position, will be used in reanalysis. Default is NA.
}
  \item{graph}{
graph or not. TRUE or FLASE
}
}

\value{
MSS returns a list of the top three maximal segmental score regions.
The first interval has the maximum score.
The second/third interval has second/third maximun score.(if exist)
An object of class "MSS" is a list containing at least the following components:

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

%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
  Lin, Y. C., Hsiao, C. L., Hsieh, A. R., Lian, I., & Fann, C. S. (2012).
  Using maximal segmental score in genome-wide association studies. \emph{Genetic epidemiology, 36(6)}, 594–601.
  \doi{10.1002/gepi.21652}

  Lin, Y., Hsieh, A., Hsiao, C. et al. (2014).
  Identifying rare and common disease associated variants in genomic data using Parkinson's disease as a model.
  \emph{J Biomed Sci} 21, 88
  \doi{10.1186/s12929-014-0088-9}

  Hsieh, A. R., Sie, J. J., Chang, C. C., Ott, J., Lian, I. B., & Fann, C. (2020).
  Maximal Segmental Score Method for Localizing Recessive Disease Variants Based on Sequence Data.
  \emph{Frontiers in genetics, 11}, 555.
  \doi{10.3389/fgene.2020.00555}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
p_value<-runif(100)
floor=0.3
MSS(p_value,floor)
}
