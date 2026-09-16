# Tests for check_pda_cache.R

## a stand-in for FEMA's listing page so no test touches the network
fake_listing_response <- function(total, status = 200L) {
  structure(
    list(
      status_code = status,
      content = charToRaw(stringr::str_c(
        "<div class=\"view-footer\">\n  Displaying 1 - 20 of ", total, "\n</div>")),
      headers = list("content-type" = "text/html; charset=UTF-8")),
    class = "response")
}

test_that("the listed total is compared against the number of cached PDFs", {
  cache_directory <- withr::local_tempdir()
  file.create(file.path(cache_directory, c("a.pdf", "b.PDF", "notes.txt")))

  testthat::local_mocked_bindings(
    GET = function(...) fake_listing_response(total = 1404),
    .package = "httr")

  expect_message(
    result <- check_pda_cache(cache_directory),
    "FEMA lists 1404 reports; the cache holds 2 PDFs")

  expect_equal(result$listed, 1404L)
  expect_equal(result$cached, 2L)
  expect_equal(result$difference, 1402L)
})

test_that("a non-200 response and a missing total both stop with a clear error", {
  cache_directory <- withr::local_tempdir()

  testthat::local_mocked_bindings(
    GET = function(...) fake_listing_response(total = 10, status = 503L),
    .package = "httr")
  expect_error(check_pda_cache(cache_directory), "returned status 503")

  testthat::local_mocked_bindings(
    GET = function(...) {
      response <- fake_listing_response(total = 10)
      response$content <- charToRaw("<div>no footer here</div>")
      response },
    .package = "httr")
  expect_error(check_pda_cache(cache_directory), "Could not find")
})

test_that("a cache directory that does not exist is an error", {
  expect_error(
    check_pda_cache(file.path(tempdir(), "does-not-exist")),
    "does not exist")
})
