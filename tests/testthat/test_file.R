
library(testthat)
library(First)



test_that("Make file function works",{

  expect_that('accident_2013.csv.bz2', equals(make_filename(2013)))

})

# expect_that(fars_read('data/accident_2013.csv.bz2'),is_a('tbl_df'))
#
# expect_error(fars_read('data/accident_2012.csv.bz2'))
#
# setwd('data')
#
# expect_that(fars_read_years(2013:2015),is_a('list'))
#
# expect_that(fars_summarize_years(2013:2015),is_a('tbl_df'))
#
# setwd('..')
