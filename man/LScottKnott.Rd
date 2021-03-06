\name{LScottKnott}
\alias{LScottKnott}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Scott-Knott test }
\description{
  Scott-Knott test to group means. This function was tested and it worked correctly for completely random design and randomized complete block design.
}
\usage{
LScottKnott(anova, which, conf.level=0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{anova}{ A \code{aov} fitted model object. }
  \item{which}{ The factor in the fitted model for be used in the test. }
  \item{conf.level}{ A numeric value between zero and one. Default is 0.95. }
}
\references{ GATES, C.E.; BILBRO, J.D. Illustration of a Cluster Analysis Method for Mean Separation. Agronomy Journal, Vol.70, May-June 1978.}
\author{ Laercio Junio da Silva \email{laerciojunio@yahoo.com.br} }
\examples{
#completely random design
require(laercio)
ldata$trat <- factor(ldata$trat)
anv <- aov(resp~trat,ldata)
anova(anv)
LScottKnott(anv,"trat")

#randomized complete block design
require(laercio)
ldata$trat <- factor(ldata$trat)
ldata$block <- factor(ldata$block)
anv <- aov(resp~trat+block,ldata)
anova(anv)
LScottKnott(anv,"trat")
LScottKnott(anv,"block")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }

