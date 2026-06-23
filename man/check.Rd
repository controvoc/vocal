\name{check}

\alias{check_variables}
\alias{check_values}
\alias{check_lonlat}
\alias{check_caps}
\alias{check_date}
\alias{check_datespan}
\alias{check_span}

\title{
Functions for compliance checking
}

\description{
Functions for compliance checking. You can check whether variable names are accepted or not, and if required variables are avaialble. You can also check whether character values are accepted and weather other variables are within specficied ranges. Furthermore you can check whether coordinates are in the country specified; whether dates are valid or whether a (date) span is within an expected limit.
}


\usage{
check_variables(x, trms, required=TRUE, suggest=TRUE)
check_values(x, trms)
check_lonlat(x)
check_caps(x, vars, minchar=5, frac=0)
check_span(x, start, end, smin=0, smax=366)
check_date(x, name, trms=NULL)
check_datespan(x, startdate, enddate, smin=0, smax=366)
}
 
\arguments{
  \item{x}{data.frame to check}
  \item{trms}{data.frame with the vocabulary. E.g. \code{accepted_values("github:controvoc/terminag")}}
  \item{required}{logical. If \code{TRUE} the presence of required variables is checked}
  \item{suggest}{logical. If \code{TRUE}, unknown variable names are reported with a
    close match from the vocabulary (Levenshtein distance, as in \code{carobiner::draft})}
  \item{name}{character. The variable name. For example, "planting_date"}
  \item{vars}{character. The variables to check}
  \item{minchar}{positive integer}
  \item{frac}{numeric}
  \item{start}{numeric}
  \item{end}{numeric}
  \item{smin}{numeric}
  \item{smax}{numeric}
  \item{startdate}{Date}
  \item{enddate}{Date}
}

\value{
data.frame
}


\examples{

set_vocabulary("github:controvoc/terminag")
trms <- vocal::accepted_variables(exclude=c("metadata", "carob-metadata"))

x <- data.frame(
	dataset_id = 1,
	country="Côte d'Ivoire",
	location = "Taï",
	admin1 = "Montagnes",
	longitude=10,
	lat=12,
	longitude = -7.6,
	latitude = 5.875,
	yield = 3969,
	yield_part = "roots",
	crop = "manioc", # should be "cassava"
	planting_date = "2030-01-01",
	harvest_date = "2035-01-01",
	N_fertilizer = 100,
	P_fertilizer = 25,
	K_fertilize = -10,
	irrigated = FALSE,
	test = 0
)

check_variables(x, trms, required=FALSE, suggest=FALSE)

check_variables(x, trms, required=FALSE, suggest=TRUE)

check_variables(x, trms, required=TRUE)

vocal::check_values(x, trms)

check_date(x, "planting_date")

check_datespan(x, "planting_date", "harvest_date")

check_lonlat(x)
}


