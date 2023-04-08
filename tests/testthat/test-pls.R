#Using variables as of 2023.04.02:
### Extract FY with prefix
### IMLS site design stores years in the accordion
test_that("PLS URL retrieval works", {
  extract <- 'fy20..'
  site <- 'https://www.imls.gov'
  xpath <- '//*[@data-ui-role="accordion"]'
  element <- 'a'
  grepl <- '*pls_fy'
  mock <- testthat::test_path('fixtures', 'PLS_sample.html') #path to the saved sample HTML file
  snap <- testthat::test_path('fixtures',
                              'PLS_URL_snapshot.txt') %>%
    readLines() #saved example output (URLs only, no names)
  pls <- get_pls_urls(url = mock,
                      site = site,
                      xpath = xpath,
                      element = element,
                      grepl = grepl,
                      extract = extract)
  names(snap) <- stringr::str_extract(snap, extract)
  expect_equal(pls, snap)
})
# Note: I feel like this is kind of all-or-nothing as a test but I'm honestly unsure what else I could do about the URL fetching specifically?
# Update: maybe break apart testing url extraction (rvest) and name extraction (stringr)?
