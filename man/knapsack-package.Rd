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
\url{https://en.wikipedia.org/wiki/Knapsack_problem}
}

\keyword{ package }

\examples{
data(knapsack_objects)

brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
greedy_knapsack(x = knapsack_objects[1:8,], W = 3500)
}
