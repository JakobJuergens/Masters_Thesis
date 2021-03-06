% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function_comparison_fourier.R
\name{fourier_zeroes}
\alias{fourier_zeroes}
\title{This function calculates the zeroes of the difference of two functions
given in terms of the fourier coefficients of the same fourier basis
based on https://math.stackexchange.com/questions/370996/roots-of-a-finite-fourier-series}
\usage{
fourier_zeroes(func_a, func_b, domain = c(0, 1))
}
\arguments{
\item{func_a:}{fd coefficient matrix for Function one}

\item{func_b:}{fd coefficient matrix for Function two}

\item{domain:}{interval to check}
}
\value{
The zeroes of the difference function
}
\description{
This function calculates the zeroes of the difference of two functions
given in terms of the fourier coefficients of the same fourier basis
based on https://math.stackexchange.com/questions/370996/roots-of-a-finite-fourier-series
}
