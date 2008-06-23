\name{LScottKnott}
\alias{LScottKnott}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Scott-Knott test }
\description{
 Scott-Knott test to group means.
}
\usage{
LScottKnott(anova, which = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{anova}{ A \code{aov} fitted model object. }
  \item{which}{ The factor in the fitted model for be used in the test. }
}
\references{ GATES, C.E.; BILBRO, J.D. Illustration of a Cluster Analysis Method for Mean Separation. Agronomy Journal, Vol.70, May-June 1978.}
\author{ Laercio Junio da Silva \email{laerciojunio@yahoo.com.br} }
\examples{
require(laercio)
ldata$trat <- as.factor(ldata$trat)
anv <- aov(resp~trat,ldata)
anova(anv)
LScottKnott(anv,"trat")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }

