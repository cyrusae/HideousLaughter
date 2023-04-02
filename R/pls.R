# Functionality relating to downloading/extracting/preparing PLS data from the IMLS site.


#' Retrieve URLs from IMLS
#'
#' @description
#' Hits the IMLS page for PLS data dumps and returns a vector of URLs we want to download.
#'
#' @details
#' Future-proofing concerns: this works as long as the IMLS keeps putting things on their website as it worked in 2023 and don't come up with a new name scheme for files. If they do the latter, consider searching for a) links to zip files and b) links closest to the string 'CSV'--it works now because the CSVs are the first/default option. Their file name scheme has been consistent since 2014, which happily enough is the timeframe TASCHA wants anyway, but if you want to go back further the parameter to change is `grepl` (consider trying `'*.zip'`, maybe).
#'
#' @param url Full address of the page on the IMLS site to retrieve download URLs from. Currently `'https://www.imls.gov/research-evaluation/data-collection/public-libraries-survey'`. (Is a parameter to allow for unit testing, if this changes we probably have bigger problems.)
#' @param site IMLS site the download URLs expect as a prefix (`'https://www.imls.gov'`). Above parenthetical applies!
#' @param xpath Node from the IMLS page to look for results in. At time of writing the one that works is `'//*[@data-ui-role="accordion"]'`. Used as `xpath` input for `rvest`.
#' @param element Element to retrieve contents of. Used as input for `rvest`. Default `'a'` (we are looking for links).
#' @param grepl Regex identifying a file that's relevant. Default `'*pls_fy'` (returns 2014-present because that happens to be how long they've been consistently using that).
#' @param extract Regex to determine name scheme for FY extraction. Default `'fy20..'` (produces results like `'fy2045'`).
#'
#' @returns A named character vector of URLs to download and their corresponding reporting years.
#' @export
#'
#' @example ${1: # pls <- get_pls_urls()}
#'
get_pls_urls <- \(url = 'https://www.imls.gov/research-evaluation/data-collection/public-libraries-survey',
                  site = 'https://www.imls.gov',
                  xpath = '//*[@data-ui-role="accordion"]',
                  element = 'a',
                  grepl = '*pls_fy',
                  extract = 'fy20..') {
  # TODO validate inputs on principle
  pls <- rvest::read_html(url) %>%
    rvest::html_nodes(xpath = xpath) %>%
    rvest::html_children() %>% #the node contains years
    rvest::html_element(element) %>% #get the first link
    rvest::html_attrs() #get the url that link refers to
  pls <- pls[grepl(grepl, pls)] #reduce to real links
  pls <- paste0(site, pls) #list of download URLs for zip files
  names(pls) <- stringr::str_extract(pls, extract) #list now has name of the FY each URL is for
  pls #return list (character vector) of URLs with their FYs as names
}
