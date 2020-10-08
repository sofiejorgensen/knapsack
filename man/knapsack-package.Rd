\name{knapsack-package}
\alias{knapsack-package}
\alias{knapsack}
\docType{package}
\title{
\packageTitle{knapsack}
}
\description{
\packageDescription{knapsack}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{knapsack}
\packageIndices{knapsack}
SHOW SOME FUNCTIONS
}
\author{
\packageAuthor{knapsack}

Maintainer: \packageMaintainer{knapsack}
}
\references{
ADD REFERENCES
}

\keyword{ package }

\examples{
knapsack_objects <-
data.frame(
  v = c(10,40,30,50),
  w = c(5,4,6,3)
)
W <- 10

brute_force_knapsack((knapsack_objects, W)
}
