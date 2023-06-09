% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ordinalbasic.R
\name{predict_ordinal}
\alias{predict_ordinal}
\title{Predict ordinal-valued tensor entries from the cumulative logistic model.}
\usage{
predict_ordinal(theta,omega,type = c("mode","mean","median"))
}
\arguments{
\item{theta}{A continuous-valued tensor (latent parameters).}

\item{omega}{The cut-off points.}

\item{type}{Type of estimations:

\code{"mode"} specifies argmax based label estimation.

\code{"mean"} specifies mean based label estimation.

\code{"median"} specifies median based label estimation.}
}
\value{
A predicted ordinal-valued tensor given latent parameters and a type of estimations.
}
\description{
Predict ordinal-valued tensor entries given latent parameters and a type of estimations.
}
\examples{
indices <- c(10,20,30)
arr <- array(runif(prod(indices),-2,2),dim = indices)
b <- c(-1.5,0,1.5)
r_predict <- predict_ordinal(arr,b,type = "mode");r_predict
}
\references{
C. Lee and M. Wang. Tensor denoising and completion based on ordinal observations. \emph{International Conference on Machine Learning (ICML)}, 2020.
}
