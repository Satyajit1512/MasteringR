% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_map_state}
\alias{fars_map_state}
\title{Make a pictoral map plotting where the accidents occured in a particular US state during a particular year.}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{a character or integer value for state id}

\item{year}{a  character or integer value for a year}
}
\value{
The function returns a plot with given rough given state boundaries and areas where accident occured in that state
during the given year and as a side effect it also print out the map plot.
}
\description{
The function reads a state number and the year and plots the accident areas in the state on a map.
If the state number entered doesnt belong to a valid state in US an error is thrown saying (\code{invalid state number state.num}) and program stops
execution
}
\details{
The function takes in state number and year, then using both and make_filename and fars_read reads in
data for particular year and then filters it out for a particular state num using dply's filter function.If a invalid
state num is entered the function throws an error and stops the program
This data is then checked for any spurious values of longitude and latitude which are replaced by NAs.
Then using the maps package's map function the US state area is plotted and using the graphics package's
points function the accident areas marked on the map.
The program will also stop execution if a data file for the particular year is not found in the current directory.This behavior is derived from
the fars_read function of the package.
If there is no accident given year in the particular state then the message (\code{no accidents to plot}) is displayed.
}
\examples{
fars_map_state(22,2013)


}
