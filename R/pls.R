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

#' Retrieve CSVs from IMLS zip file
#'
#' @description
#' Download a single zip file from the IMLS website, extract only the contents that are CSV files, identify and rename the outlet and administrative entity PLS responses, and return the paths to those files.
#'
#' @details
#' Development concerns: Current use of `here` is more brittle than I want it to be but I haven't figured out what the better long-term way to handle that is. Currently deletes the files that aren't the original zip or the desirable raw CSVs on cleanup.
#'
#' @param url URL leading to a single zip of CSV files on the IMLS website (see `get_pls_urls()`).
#' @param extract Regex to determine name scheme for FY extraction. Default `'fy20..'` (produces results like `'fy2045'`).
#' @param here String describing the intended destination path. Default `'data/raw/PLS_csvs'`. Needs refinement as a feature.
#'
#' @returns A named character vector of length 2 containing the paths to the admin and outlet PLS responses.
#' @export
#'
get_pls_zip <- \(url = url,
                 extract = 'fy20..',
                 here = 'data/raw/PLS_csvs') {
  assertthat::is.string(url) #check for receiving one (1) url
  assertthat::assert_that(is.character(here)) #check for viable path requests
  if (is.null(names(url))) { #make sure we have a FY name
    fy <- stringr::str_extract(url, extract)
    assertthat::assert_that(nchar(fy) == nchar(extract))
    names(url) <- fy
  }
  fy <- names(url)
  fp <- paste0(here::here(here), '/', fy) #subdirectory named for the FY
  if (!dir.exists(fp)) {
    dir.create(fp)
  } #create subdirectory if needed
  assertthat::is.writeable(fp) #make sure it is usable now
  zipfile <- paste0(fp, '/', fy, '.zip') #filename for zip download
  if (!file.exists(zipfile)) download.file(url = url,
                                           destfile = zipfile)
  assertthat::is.readable(zipfile) #is downloaded zip readable?
  zip_contents <- grep('*.csv$', #find only the CSV files
                       unzip(zipfile = zipfile, list = TRUE)$Name,
                       ignore.case = TRUE, value = TRUE)
  unzip(zipfile = zipfile, files = zip_contents,
        exdir = fp) #put the CSV files in the /fy20XX/ directory
  zip_contents <- grep('*.csv$', #get paths to the unzipped CSVs
                       list.files(fp, full.names = TRUE),
                       ignore.case = TRUE, value = TRUE)
  assertthat::assert_that(length(zip_contents) == 3) #make sure there are specifically three CSV files here
  zip_nrows <- check_nrows(files = zip_contents)
  zip_results <- data.table::data.table(path = zip_contents,
                                        nrows = zip_nrows)
  zip_results <- zip_results[nrows != min(zip_results$nrows), ] #remove the shortest CSV file (will be the state results)
  zip_results[nrows == max(zip_results$nrows), #longest = outlets
              filename := paste0(fp, '/pls_outlet_', fy, '.csv')]
  zip_results[nrows == min(zip_results$nrows), #remaining = administrative entities
              filename := paste0(fp, '/pls_admin_', fy, '.csv')]
  file.rename(from = zip_results$path,
              to = zip_results$filename)
  res <- zip_results$filename #return the relevant files
  fil <- list.files(path = fp, full.names = TRUE) %>%
    setdiff(c(res, zipfile)) #identify other contents of FY folder that aren't the zip or the desirable CSVs
  file.remove(fil) #remove them
  names(res) <- stringr::str_extract(res, extract)
  res
}
