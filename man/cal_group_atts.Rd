% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subfunctions.R
\name{cal_group_atts}
\alias{cal_group_atts}
\title{calculate group wise atts}
\usage{
cal_group_atts(x, tname, gname, unitname)
}
\description{
This function is applied to the dataframe for the treatment effects. 
It is applied to the treatment effects of all units separately (using sapply).
It then returns a dataframe including all the averaged treatment effects for
each id.
}
\keyword{internal}
