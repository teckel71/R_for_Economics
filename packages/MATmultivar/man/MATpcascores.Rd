% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MATpcascores.R
\name{MATpcascores}
\alias{MATpcascores}
\title{MATpcascores: Obtención de puntuaciones de componentes principales}
\usage{
MATpcascores(data, pca_object, num_comp, num_cases = 0, ...)
}
\arguments{
\item{data}{Dataframe original.}

\item{pca_object}{Objeto resultante de prcomp().}

\item{num_comp}{Número de componentes a extraer.}

\item{num_cases}{Número de casos a mostrar.}
}
\value{
Lista con el dataframe de puntuaciones y tabla formateada.
}
\description{
Calcula y devuelve puntuaciones de componentes principales de un PCA previamente realizado.
}
