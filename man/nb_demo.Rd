% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nb_demo.R
\name{nb_demo}
\alias{nb_demo}
\title{calculate the Value Difference Metric}
\usage{
nb_demo(train_X, train_y, test_x)
}
\arguments{
\item{train_X, }{a data.frame, the feature variables}

\item{train_y, }{string or factor, the target variable}

\item{test_x, }{a list or a data.frame including only one row}
}
\value{
a list including all the components of the naive bayes
}
\description{
calculate the Value Difference Metric
}
\details{
This function calculates the naive bayes probability for each level of the target variable.
The detailed calculation is shown in the print method.

The output of print.nb_demo is the latex version of the calculation process,
which can be pasted to a latex document.
}
\examples{
results <- nb_demo(xigua3.0[, -c(1, 10)], xigua3.0[, 10], list("青绿", "蜷缩", "浊响", "清晰", "凹陷", "硬滑", 0.697, 0.460))
results

}
