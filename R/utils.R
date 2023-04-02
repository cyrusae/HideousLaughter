
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
  n <- data.table::fread(file = filename, select = 1L) %>%
    nrow()
}
