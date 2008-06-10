\name{LTukey}
\alias{LTukey}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Tukey test }
\description{
  Tukey teste to compare means.
}
\usage{
LTukey(anova, which = "", conf.level = 0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{anova}{ A \code{aov} fitted model object. }
  \item{which}{ A term in the fitted model for be used in the test. Defaults to all the terms. }
  \item{conf.level}{ A numeric value between zero and one. Default is 0.95. }
}
\author{ Laercio Junio da Silva \email{laerciojunio@yahoo.com.br} }


\examples{
require(laercio)
anv <- aov(mean~factor,ldata)
LTukey(anv)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }

