#' Read csv data from file
#'
#' This function reads data from a csv file or a zipped file with csv data,if it exists.
#' If the file doesnt exist it gives an error message
#' saying the file \code{file filename does not exist} and stops the execution.
#'
#' @param filename character string containing Path to the file if file is not stored in current directory.
#'        filename if the csv file is stored in current directory.
#'
#' @return This function returns a data frame containing data from the file read.
#'
#' @details The function is a wrapper for read_csv from the readr package. The function throws an error if the file
#' with given filename is not found and stops program execution.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' fars_read('myfile.csv.bz')
#' fars_read('myfile.csv')
#'
#' @export
fars_read <- function(filename) {
        print(filename)
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}


#' Make csv.zip type filename from a year
#'
#' This function reads a year (\code{character,integer}) and creates a filename for a csv type zipped file containing the accident  data
#' for the year.
#
#' @param year a integer,character value for the year under consideration
#'
#' @return The function returns a string vector containing the filename.The filename is of form (\code{accident_year.csv.bz2})
#'
#' @details The function offers a convinient way to create file name to search for revelant data files
#'
#' @examples
#' make_filename(2013)
#' make_filename('2013')
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Make a list of dataframe containing month and year data for accidents.
#'
#' This function reads a vector (\code{character,integer}) of years and returns a list of dataframes containing the
#' month of accident for each accident in the given year.If for any reason the program is unable to read
#' data for a paricular year an warning (\code{invalid year: year}) is displayed and a NULL is returned for that year's value
#
#' @param years a  character or integer vector contaning years
#'
#' @return The function returns a list of dataframes one for each year of data considered .Each dataframe has two columns
#' corresponding to the month and year for each accident for that year.
#'
#' @details The function takes in a vector of years.It first uses the make_filename function to create a filename .The looks for a file with said filename
#' and reads data from it using fars_read, to retain only the month and year column before moving onto next year.
#' As described if the program is unable to read data for particular year it throws an warning message and returns a NULL for that year
#'
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select
#'
#' @examples
#' fars_read_years(c('2013','2014,'2015'))
#' fars_read_years(2013:2015)
#'
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Make a data.frame containing number of accidents in a given month for each year.
#'
#' This function reads a vector (\code{character,integer}) of years and returns a dataframe summarizing total number of accidents
#' for each month for given year in set of years. A warning message will be thrown if the program cannot
#' find data file for a particular year.
#
#' @param years a  character or integer vector contaning years
#'
#' @return The function returns a dataframe with columns as years and rows as number of accidents during each month for that year.
#'
#' @details The function takes in a vector of years.Uses fars_read_year to create a list of dataframes containing accident month
#' and year data.Groups data by year and month and then summarizes it using dplyr functions and then creates dataframe with months as rows and a column
#' for each year using spread function.A warning message will be thrown if the program cannot
#' find data file for a particular year (\code{invalid year: year}). This behavior is derived from the fars_read_years function
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows group_by summarize n
#' @importFrom tidyr spread
#'
#' @examples
#' fars_summarize_years(c('2013','2014,'2015'))
#' fars_summarize_years(2013:2015)
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}


#' Make a pictoral map plotting where the accidents occured in a particular US state during a particular year.
#'
#' The function reads a state number and the year and plots the accident areas in the state on a map.
#' If the state number entered doesnt belong to a valid state in US an error is thrown saying (\code{invalid state number state.num}) and program stops
#' execution
#
#' @param year a  character or integer value for a year
#' @param state.num a character or integer value for state id
#'
#' @return The function returns a plot with given rough given state boundaries and areas where accident occured in that state
#' during the given year and as a side effect it also print out the map plot.
#'
#' @details The function takes in state number and year, then using both and make_filename and fars_read reads in
#' data for particular year and then filters it out for a particular state num using dply's filter function.If a invalid
#' state num is entered the function throws an error and stops the program
#' This data is then checked for any spurious values of longitude and latitude which are replaced by NAs.
#' Then using the maps package's map function the US state area is plotted and using the graphics package's
#' points function the accident areas marked on the map.
#' The program will also stop execution if a data file for the particular year is not found in the current directory.This behavior is derived from
#' the fars_read function of the package.
#' If there is no accident given year in the particular state then the message (\code{no accidents to plot}) is displayed.
#'
#'
#' @importFrom graphics points
#' @importFrom dplyr filter
#' @importFrom maps map
#'
#' @examples
#' fars_map_state('22','2013')
#' fars_map_state(22,2015)
#'
#' @export

fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)
        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
