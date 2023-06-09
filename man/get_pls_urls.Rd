% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pls.R
\name{get_pls_urls}
\alias{get_pls_urls}
\title{Retrieve URLs from IMLS}
\usage{
get_pls_urls(
  url =
    "https://www.imls.gov/research-evaluation/data-collection/public-libraries-survey",
  site = "https://www.imls.gov",
  xpath = "//*[@data-ui-role=\\"accordion\\"]",
  element = "a",
  grepl = "*pls_fy",
  extract = "fy20.."
)
}
\arguments{
\item{url}{Full address of the page on the IMLS site to retrieve download URLs from. Currently \code{'https://www.imls.gov/research-evaluation/data-collection/public-libraries-survey'}. (Is a parameter to allow for unit testing, if this changes we probably have bigger problems.)}

\item{site}{IMLS site the download URLs expect as a prefix (\code{'https://www.imls.gov'}). Above parenthetical applies!}

\item{xpath}{Node from the IMLS page to look for results in. At time of writing the one that works is \code{'//*[@data-ui-role="accordion"]'}. Used as \code{xpath} input for \code{rvest}.}

\item{element}{Element to retrieve contents of. Used as input for \code{rvest}. Default \code{'a'} (we are looking for links).}

\item{grepl}{Regex identifying a file that's relevant. Default \code{'*pls_fy'} (returns 2014-present because that happens to be how long they've been consistently using that).}

\item{extract}{Regex to determine name scheme for FY extraction. Default \code{'fy20..'} (produces results like \code{'fy2045'}).}
}
\value{
A named character vector of URLs to download and their corresponding reporting years.
}
\description{
Hits the IMLS page for PLS data dumps and returns a vector of URLs we want to download.
}
\details{
Future-proofing concerns: this works as long as the IMLS keeps putting things on their website as it worked in 2023 and don't come up with a new name scheme for files. If they do the latter, consider searching for a) links to zip files and b) links closest to the string 'CSV'--it works now because the CSVs are the first/default option. Their file name scheme has been consistent since 2014, which happily enough is the timeframe TASCHA wants anyway, but if you want to go back further the parameter to change is \code{grepl} (consider trying \code{'*.zip'}, maybe).
}
