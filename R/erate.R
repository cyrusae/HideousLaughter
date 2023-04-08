# Functionality directly relating to downloading/sanitizing/filtering E-Rate data from USAC to make it bearable to deal with.

#TODO: Fill out Roxygen skeletons
#TODO: change this to handle the parameters needed after (filtering, setting an arbitrarily high limit to get around the default cap on API requests)
#' Title
#'
#' @param dataset
#' @param suffix
#'
#' @return
#' @export
#'
#' @examples
build_usac_url <- \(dataset, suffix = '.csv') {
  prefix <- 'https://opendata.usac.org/resource/'
  res <- paste0(prefix, dataset, suffix)
}

# TODO: wrapper that responds to human-readable names instead, because I can:
#' Title
#'
#' @param name
#' @param suffix
#'
#' @return
#' @export
#'
#' @examples
build_usac_url_named <- \(name, suffix = '.csv') {
  # match name to dataset here
  res <- build_usac_url(dataset = dataset, suffix = suffix)
}
