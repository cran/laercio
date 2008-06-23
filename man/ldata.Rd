\name{ldata}
\alias{ldata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Data for examples }
\description{
  Data only for to use in the examples.
}
\author{ Laercio Junio da Silva \email{laerciojunio@yahoo.com.br} }

\examples{
require(laercio)
ldata$trat <- as.factor(ldata$trat)
anv <- aov(resp~trat,ldata)
anova(anv)
LTukey(anv,"trat")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
