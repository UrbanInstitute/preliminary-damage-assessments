#' @title Compare the Local PDF Cache Against the Number of Reports FEMA Lists
#'
#' @description Compares the FEMA-reported number of reports 
#'   to the number of PDF files in `cache_directory`. 
#'
#' @details The two numbers are not guaranteed to match exactly even when the
#'   cache is complete. FEMA's count is the number of entries in the listing,
#'   while the cache holds one file per unique PDF. They can differ when two
#'   listing entries link to the same PDF, when a report was removed from the
#'   site but its file is still on disk, or when a download failed. Treat a
#'   match as reassuring and a large mismatch as a reason to run `scrape_pda_pdfs()`.
#'
#' @param cache_directory The folder where `scrape_pda_pdfs()` writes PDFs.
#'
#' @return A tibble with one row and three columns: `listed` (the count FEMA
#'   shows), `cached` (PDF files in `cache_directory`), and `difference`
#'   (`listed - cached`; positive means the cache probably has reports missing).
#' @export
#'
#' @examples
#' \dontrun{
#' check_pda_cache(cache_directory = file.path("data", "pdfs"))
#' }
check_pda_cache = function(cache_directory) {

  if (!dir.exists(cache_directory)) {
    stop(
      stringr::str_c("cache_directory does not exist: ", cache_directory),
      call. = FALSE) }

  ## the bare listing URL is used rather than "?page=0" because FEMA's CDN has
  ## served a weeks-old copy of the "?page=0" version; see scrape_pda_pdfs()
  listing_url = "https://www.fema.gov/disaster/how-declared/preliminary-damage-assessments/reports"

  response = httr::GET(
    listing_url,
    httr::add_headers(
      "User-Agent" = browser_user_agent(),
      "Accept" = "text/html",
      "Accept-Language" = "en-US"),
    httr::timeout(120))

  if (httr::status_code(response) != 200) {
    stop(
      stringr::str_c(
        "FEMA's listing page returned status ", httr::status_code(response),
        ". Wait a few minutes and try again."),
      call. = FALSE) }

  listed = httr::content(response, as = "text", encoding = "UTF-8") %>%
    stringr::str_extract("Displaying\\s+\\d+\\s*-\\s*\\d+\\s+of\\s+(\\d+)", group = 1) %>%
    as.integer()

  if (is.na(listed)) {
    stop(
      "Could not find the \"Displaying 1 - 20 of N\" total on FEMA's listing ",
      "page. The page layout may have changed.",
      call. = FALSE) }

  cached = length(
    list.files(cache_directory, recursive = TRUE, pattern = "(?i)pdf$"))

  result = tibble::tibble(
    listed = listed,
    cached = cached,
    difference = listed - cached)

  message(
    "FEMA lists ", listed, " reports; the cache holds ", cached, " PDFs ",
    "(difference of ", result$difference, "). The two counts can differ ",
    "slightly even when the cache is complete; run scrape_pda_pdfs() to find ",
    "exactly which reports are missing.")

  result
}
