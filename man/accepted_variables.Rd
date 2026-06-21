\name{accepted_variables}

\alias{accepted_variables}

\title{
Get accepted variables
}

\description{
Get a data.frame with accepted variable names
}

\usage{
accepted_variables(include=NULL, exclude=NULL)
}
 
\arguments{
  \item{include}{character or NULL. Can be used to use a subset of variable groups}
  \item{exclude}{character or NULL. Can be used to exclude variable groups}
}

\value{
data.frame
}

\examples{
set_vocabulary("github:controvoc/terminag")
v <- accepted_variables("crop")
head(v)
}
