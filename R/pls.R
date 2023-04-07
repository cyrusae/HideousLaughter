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

#' Process PLS CSV file (internal)
#'
#' @noRd
#'
#' @param file Path to file (inherited from wrapper function).
#' @param response Either 'outlet' or 'admin'.
#' @param fy Expects `fy20..` format used elsewhere.
#' @param here See general concerns with `here` usage.
#'
#' @returns The filepath for the single successfully written file.
#' @export
#'
get_pls_csv <- \(file, response, fy, here) {
  assertthat::is.readable(file) #is the file readable?
  assertthat::is.writeable(here::here(here)) #is the destination writable?
  assertthat::is.string(fy) #is the input fy coherent?
  assertthat::is.string(response) #do we know what the response is?
  dest <- paste0(here::here(here), '/pls_',
                 response, '_', fy, '.csv')
  dt <- data.table::fread(file = file)
  dt[dt == -9] <- NA #Remove suppressed data
  dt[dt == -4] <- NA #Remove for closures
  dt[dt == -3] <- NA #Remove for closures
  dt[dt == -1] <- NA #Remove unanswered questions
  dt[dt == 'M'] <- NA #Remove missing values
  if ('MICROF' %in% names(dt)) dt[MICROF == 'N', MICROF := NA] #NA for the MICROF field only
  if ('RSTATUS' %in% names(dt)) dt[RSTATUS == 3, RSTATUS := NA] #remove nonrespondents
  data.table::fwrite(dt, file = dest)
  assertthat::is.readable(path = dest)
  dest #return successfully-written file
}

#' Retrieve CSVs from IMLS zip file
#'
#' @description
#' Download a single zip file from the IMLS website (if needed), extract only the contents that are CSV files, identify and rename the outlet and administrative entity PLS responses, and return the paths to those files while deleting the intermediary files.
#'
#' @details
#' Development concerns: Current use of `here` is more brittle than I want it to be but I haven't figured out what the better long-term way to handle that is.
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
  assertthat::is.string(url) #Did we get a URL that we can ping
  assertthat::is.string(here) #Is the desired path viable
  if (is.null(names(url))) {
    fy <- stringr::str_extract(url, extract)
    assertthat::assert_that(length(fy) == length(extract))
    names(url) <- fy
  }
  fy <- names(url) #Get the FY value out
  fp <- paste0(here::here(here), '/', fy)
  if (!dir.exists(fp)) dir.create(fp) #Create directory if needed
  #Note: still deciding whether we're keeping the zip files and deleting everything else or what
  #TODO Add error handling once we're sure of cleanup steps
  assertthat::is.writeable(fp) #Can we write to the directory
  zipfile <- paste0(fp, '/', fy, '.zip')
  if (!file.exists(zipfile)) {
    download.file(url = url, destfile = zipfile, quiet = TRUE)
  } #Download a file if it doesn't appear to exist yet
  assertthat::is.readable(zipfile) #Did we get the zip successfully (or have it already)?
  zip_contents <- grep('\\w+\\.csv$', #find only the CSV files
                       unzip(zipfile = zipfile,
                             list = TRUE)$Name, #Names only
                       ignore.case = TRUE, value = TRUE)
  unzip(zipfile = zipfile, files = zip_contents,
        exdir = fp) #put the CSV files in the /fy20XX/ directory
  zip_contents <- list.files(path = fp, pattern = '\\w+\\.csv$',
                             full.names = TRUE, recursive = TRUE,
                             include.dirs = TRUE)
  assertthat::assert_that(length(zip_contents) == 3) #make sure there are specifically three CSV files here
  zip_nrows <- check_nrows(files = zip_contents)
  zip_results <- data.table::data.table(
    path = zip_contents,
    nrows = zip_nrows
  ) #track features about the files that we'll need in a bit
  zip_results <- zip_results[nrows != min(nrows), ] #remove the states file from consideration
  zip_results[nrows == max(nrows),
              response := 'outlet'] #largest file will be outlets
  zip_results[nrows == min(nrows),
              response := 'admin'] #remaining will be administrative entities
  csvs <- furrr::future_map2_chr(.x = zip_results$path,
                                 .y = zip_results$response,
                                 .f = get_pls_csv,
                                 fy = fy, here = here)
  # Clean up
  process_files <- list.files(path = fp, full.names = TRUE) %>%
    setdiff(zipfile) #Get everything but the original zip
  unlink(process_files, recursive = TRUE) #Delete
  csvs #return paths to created files
}

#' Use list of PLS URLs
#'
#' @param pls Character vector of PLS URLs (see `get_pls_urls()`); expects to find names in `fy20..` format also.
#' @param extract Fallback regex to get the names described above.
#' @param here String describing the intended destination path. Default `'data/raw/PLS_csvs'`. Needs refinement as a feature.
#'
#' @return Returns a list of the filenames being produced.
#' @export
#'
get_pls_data <- \(pls,
                  extract = 'fy20..',
                  here = 'data/raw/PLS_csvs') {
  files <- furrr::future_map(.x = pls, .f = get_pls_data,
                             extract = extract, here = here)
}

#' Wrapper function for all PLS processing
#'
#' @description
#' Check the IMLS website for PLS URLs, feed the resulting URLs into the processing functions, and return a list of usable files. Wraps all of the incremental PLS-processing functions.
#'
#' @inheritParams get_pls_urls
#'
#' @param here String describing the intended destination path. Default `'data/raw/PLS_csvs'`. Needs refinement as a feature.
#'
#' @returns A list of file paths to the processed files.
#' @export
#'
get_pls <- \(here = 'data/raw/PLS_csvs',
             url = 'https://www.imls.gov/research-evaluation/data-collection/public-libraries-survey',
             site = 'https://www.imls.gov',
             xpath = '//*[@data-ui-role="accordion"]',
             element = 'a',
             grepl = '*pls_fy',
             extract = 'fy20..') {
  # TODO:
  ### 1) test this functionality;
  ### 2) integrate checking for duplicated data (default to not running through any named FY that has both of its relevant CSVs in the top-level `here` directory)
  get_pls_urls(url = url, site = site, xpath = xpath,
               element = element, grepl = grepl,
               extract = extract) %>%
    get_pls_data(extract = extract, here = here)
}
