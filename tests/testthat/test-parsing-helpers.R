# Tests for the extraction and quality-check helpers in
# get_preliminary_damage_assessments.R

test_that("a named date is parsed with or without a comma", {
  expect_equal(
    date_string_to_date(c("January 01 2000", "January 1, 2000", "December 31, 2019")),
    as.Date(c("2000-01-01", "2000-01-01", "2019-12-31")))
})

test_that("a month name inside another word is not read as a month", {
  ## "Maryland" contains "May"; the date must still parse correctly
  expect_equal(
    date_string_to_date("Maryland Severe Storms Declared May 5, 2020"),
    as.Date("2020-05-05"))
  ## and a title with no date yields NA rather than a fabricated one
  expect_true(is.na(date_string_to_date("Maryland Severe Storms")))
})

test_that("extract_value returns the text between two labels", {
  text <- "Destroyed: 12 Major Damage: 45 Minor Damage: 100"
  expect_equal(extract_value(text, "Destroyed:", "Major Damage:"), "12")
  expect_equal(extract_value(text, "Major Damage:", "Minor Damage:"), "45")
})

test_that("extract_value tolerates a missing colon and an alternation", {
  ## most reports print the label with no colon at all
  expect_equal(
    extract_value("Destroyed 12 Major Damage 45", "Destroyed:", "Major Damage:"),
    "12")
  ## several call sites pass alternations as terms
  expect_equal(
    extract_value(
      "poverty households: 18 elderly households: 20",
      "poverty households:|low income households:",
      "ownership households:|elderly households:"),
    "18")
})

test_that("PDF destinations are unique, cleaned, and hashed only when needed", {
  urls <- c(
    "https://www.fema.gov/files/report-one.pdf",
    ## a query string must not survive into the filename
    "https://www.fema.gov/files/report-two.pdf?v=2",
    ## two different URLs reducing to one basename must both be kept
    "https://www.fema.gov/a/shared-name.pdf",
    "https://www.fema.gov/b/shared-name.pdf")

  destinations <- suppressMessages(resolve_pdf_destinations(urls))

  expect_equal(nrow(destinations), 4)
  expect_equal(anyDuplicated(destinations$destination_file), 0)
  expect_true("report-one.pdf" %in% destinations$destination_file)
  expect_true("report-two.pdf" %in% destinations$destination_file)
  expect_true(all(stringr::str_detect(destinations$destination_file, "\\.pdf$")))
  ## the colliding pair are both hashed
  shared <- destinations %>%
    dplyr::filter(stringr::str_detect(url, "shared-name"))
  expect_true(all(stringr::str_detect(shared$destination_file, "^shared-name-")))
})

test_that("a duplicated disaster number is corrected from the report text", {
  reports <- tibble::tibble(
    path = c("a.pdf", "b.pdf"),
    text = c("... FEMA-4001 ...", "... FEMA-4002 ..."),
    ## both rows arrived under 4001; b's own text says 4002
    disaster_number = c("4001", "4001"),
    disaster_number_filename = c(NA_character_, NA_character_))

  corrected <- correct_duplicate_disaster_numbers(reports)

  expect_equal(corrected$disaster_number, c("4001", "4002"))
})

test_that("a duplicated number disagreeing with its filename falls back to it", {
  ## the FEMA-4599-DR (Oregon) case: the body prints another disaster's number
  reports <- tibble::tibble(
    path = c("FEMA4595DRKY.pdf", "FEMA4599DROR.pdf"),
    text = c("... FEMA-4595 ...", "... FEMA-4595 ..."),
    disaster_number = c("4595", "4595"),
    disaster_number_filename = c("4595", "4599"))

  corrected <- correct_duplicate_disaster_numbers(reports)

  expect_equal(corrected$disaster_number, c("4595", "4599"))
})

test_that("quality checks flag impossible values and leave clean rows alone", {
  reports <- tibble::tibble(
    path = c("a.pdf", "b.pdf", "c.pdf"),
    disaster_number = c("4001", "4002", "4003"),
    event_type = "approved",
    event_title = "title",
    event_native_flag = 0,
    text = "text",
    ia_requested = 1,
    pa_requested = 1,
    ## row b: negative count; row c: percentage above 100
    ia_residences_impacted = c(100, -5, 200),
    ia_households_poverty_percent = c(20, 30, 400),
    ## row b: a dollar total small enough to be a footnote marker
    pa_cost_estimate_total = c(1e6, 7, 2e6))

  checked <- check_pda_quality(reports)

  expect_true(is.na(checked$warnings[1]))
  expect_match(checked$warnings[2], "negative")
  expect_match(checked$warnings[2], "footnote number")
  expect_match(checked$warnings[3], "outside 0-100")
})

test_that("damage categories summing past the impacted total are flagged", {
  reports <- tibble::tibble(
    path = c("a.pdf", "b.pdf"),
    disaster_number = c("4001", "4002"),
    event_type = "approved",
    event_title = "title",
    event_native_flag = 0,
    text = "text",
    ia_requested = 1,
    pa_requested = 1,
    ia_residences_impacted = c(100, 100),
    ia_residences_destroyed = c(10, 80),
    ia_residences_major_damage = c(20, 80),
    ia_residences_minor_damage = c(30, 80),
    ia_residences_affected = c(40, 80))

  checked <- check_pda_quality(reports)

  expect_true(is.na(checked$warnings[1]))
  expect_match(checked$warnings[2], "sum to more than the stated total")
})

test_that("a duplicated source path and a shared approved number are flagged", {
  reports <- tibble::tibble(
    path = c("a.pdf", "a.pdf", "b.pdf"),
    disaster_number = c("4001", "4001", "4001"),
    event_type = "approved",
    event_title = "title",
    event_native_flag = 0,
    text = "text",
    ia_requested = 1,
    pa_requested = 1,
    ia_residences_impacted = c(100, 100, 100))

  checked <- check_pda_quality(reports)

  expect_true(all(stringr::str_detect(
    checked$warnings[1:2], "parsed into more than one row")))
  expect_true(all(stringr::str_detect(
    checked$warnings, "maps to more than one approved PDA")))
})
