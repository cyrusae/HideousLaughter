# Functionality that doesn't belong in any other `.R` script file and is good to have around.

#' Number of rows in a fread-legible file
#'
#' @description
#' Uses `data.table::fread()` to determine `nrow` for a file without actually loading all of its contents.
#'
#' @param file Path to the file being evaluated.
#'
#' @returns Returns an integer `nrow` value.
#' @export
#'
check_nrow <- \(file = filename) {
  assertthat::is.readable(file) #check filename
  n <- data.table::fread(file = file, select = 1L) %>%
    nrow()
}

#' Vectorized `check_nrow()`
#'
#' @description
#' Uses `data.table::fread()` to determine `nrow` for multiple files without actually loading their contents. Returns a vector of results in the original order.
#'
#' @param files Character vector of one or more file paths. (If it's only one, you should probably be using `check_nrow()` on its own.)
#'
#' @returns Returns an integer vector corresponding to the `nrow` results in order.
#' @export
#'
check_nrows <- \(files = c()) {
  assertthat::assert_that(is.vector(files, mode = 'character'))
#  ns <- purrr::map_int(.x = files, .f = check_nrow)
  ns <- furrr::future_map_int(.x = files, .f = check_nrow)
}
