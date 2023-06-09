% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pls.R
\name{get_pls}
\alias{get_pls}
\title{Wrapper function for all PLS processing}
\usage{
get_pls(
  here = "data/raw/PLS_csvs",
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
\item{here}{String describing the intended destination path. Default \code{'data/raw/PLS_csvs'}. Needs refinement as a feature.}

\item{url}{Full address of the page on the IMLS site to retrieve download URLs from. Currently \code{'https://www.imls.gov/research-evaluation/data-collection/public-libraries-survey'}. (Is a parameter to allow for unit testing, if this changes we probably have bigger problems.)}

\item{site}{IMLS site the download URLs expect as a prefix (\code{'https://www.imls.gov'}). Above parenthetical applies!}

\item{xpath}{Node from the IMLS page to look for results in. At time of writing the one that works is \code{'//*[@data-ui-role="accordion"]'}. Used as \code{xpath} input for \code{rvest}.}

\item{element}{Element to retrieve contents of. Used as input for \code{rvest}. Default \code{'a'} (we are looking for links).}

\item{grepl}{Regex identifying a file that's relevant. Default \code{'*pls_fy'} (returns 2014-present because that happens to be how long they've been consistently using that).}

\item{extract}{Regex to determine name scheme for FY extraction. Default \code{'fy20..'} (produces results like \code{'fy2045'}).}
}
\value{
A list of file paths to the processed files.
}
\description{
Check the IMLS website for PLS URLs, feed the resulting URLs into the processing functions, and return a list of usable files. Wraps all of the incremental PLS-processing functions.
}
