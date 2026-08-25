# Tests for transform_pda_counties.R

## A minimal stand-in for the report-level output of
## `get_preliminary_damage_assessments(join_outcomes = FALSE)`: the columns the
## transformation reads, and nothing else.
pda_fixture <- function(countywide, state_name = "Kentucky", warnings = NA_character_) {
  tibble::tibble(
    pda_path = stringr::str_c("report_", seq_along(countywide), ".pdf"),
    pda_state_name = state_name,
    pda_pa_per_capita_impact_countywide = countywide,
    pda_warnings = warnings)
}

test_that("a county listing becomes one row per county, with FIPS codes", {
  counties <- suppressWarnings(transform_pda_counties(pda_fixture(
    "Barren County 14.84 Bullitt County 8.56 and Butler County 87.53.")))

  expect_equal(nrow(counties), 3)
  expect_equal(
    counties$pda_county_name,
    c("Barren County", "Bullitt County", "Butler County"))
  expect_equal(counties$pda_county_geoid, c("21009", "21029", "21031"))
  expect_equal(counties$pda_pa_per_capita_impact_county, c(14.84, 8.56, 87.53))
  expect_equal(unique(counties$pda_geography_type), "county")
})

test_that("the sentence some reports append to the listing is not read as a county", {
  counties <- suppressWarnings(transform_pda_counties(pda_fixture(
    stringr::str_c(
      "Barren County 14.84 and Butler County 0. ",
      "Joint PDAs have not been completed in the counties with a 0 per capita."))))

  expect_equal(nrow(counties), 2)
  expect_equal(counties$pda_county_name_reported, c("Barren County", "Butler County"))
})

test_that("a value of zero is preserved and flagged", {
  counties <- suppressWarnings(transform_pda_counties(pda_fixture(
    "Barren County 14.84 and Butler County 0.00.")))

  expect_equal(counties$pda_pa_per_capita_impact_county, c(14.84, 0))
  expect_true(is.na(counties$pda_warnings[1]))
  expect_match(counties$pda_warnings[2], "per capita impact of 0")
  expect_match(counties$pda_warnings[2], "preserved as printed")
})

test_that("a county listed twice returns two rows, both flagged", {
  counties <- suppressWarnings(transform_pda_counties(pda_fixture(
    stringr::str_c(
      "First Incident - Barren County 6.34 and Butler County 12.54. ",
      "Second Incident - Barren County 2.28 and Butler County 0.55"))))

  barren <- dplyr::filter(counties, pda_county_name == "Barren County")
  expect_equal(nrow(barren), 2)
  expect_equal(barren$pda_pa_per_capita_impact_county, c(6.34, 2.28))
  expect_true(all(stringr::str_detect(barren$pda_warnings, "listed more than once")))
})

test_that("warnings already on the record are kept alongside the new ones", {
  counties <- suppressWarnings(transform_pda_counties(pda_fixture(
    "Butler County 0.00", warnings = "an existing problem")))

  expect_match(counties$pda_warnings, "an existing problem")
  expect_match(counties$pda_warnings, "per capita impact of 0")
})

test_that("a listing that ran past the end of its section is flagged", {
  counties <- suppressWarnings(transform_pda_counties(pda_fixture(
    stringr::str_c(
      "Barren County 14.84 Primary Impact Damage to utilities ",
      "Total Public Assistance cost estimate 39614"),
    state_name = "West Virginia")))

  bled <- dplyr::filter(
    counties, stringr::str_detect(dplyr::coalesce(pda_warnings, ""), "ran past the end"))
  expect_equal(nrow(bled), 1)
  expect_equal(bled$pda_pa_per_capita_impact_county, 39614)
})

test_that("places that are not counties are typed and left without a FIPS code", {
  counties <- suppressWarnings(transform_pda_counties(pda_fixture(
    c("Lower Kuskokwim REAA 12.00", "the Gila River Indian Community 43.00"),
    state_name = c("Alaska", "Arizona"))))

  expect_equal(counties$pda_geography_type, c("education area", "tribal entity"))
  expect_true(all(is.na(counties$pda_county_geoid)))
  ## the leading "the" belongs to the sentence, not to the name
  expect_equal(counties$pda_county_name[2], "Gila River Indian Community")
})

test_that("the spellings FEMA and the Census disagree on still match", {
  counties <- suppressWarnings(transform_pda_counties(pda_fixture(
    c("Prince George's County 4.00", "Orocovis Municipality 9.00",
      "Blount 3.00", "City and Borough of Juneau 20.00",
      "Saint Clair County 7.00"),
    state_name = c("Maryland", "Puerto Rico", "Alabama", "Alaska", "Missouri"))))

  expect_equal(
    counties$pda_county_geoid,
    c("24033", "72107", "01009", "02110", "29185"))
  expect_equal(
    counties$pda_geography_type,
    c("county", "municipio", "county", "borough", "county"))
})

test_that("reports with no county listing are dropped, with a warning", {
  input <- pda_fixture(c("Barren County 14.84", NA_character_))

  expect_warning(
    counties <- transform_pda_counties(input),
    "include no countywide breakdown")
  expect_equal(nrow(counties), 1)
})

test_that("the joined output's FEMA state name is used where present", {
  input <- tibble::tibble(
    fema_state_name = "Kentucky",
    pda_path = "report_1.pdf",
    pda_pa_per_capita_impact_countywide = "Barren County 14.84",
    pda_warnings = NA_character_)

  counties <- suppressWarnings(transform_pda_counties(input))

  expect_true("pda_county_geoid" %in% names(counties))
  expect_true("pda_geography_type" %in% names(counties))
  expect_equal(counties$pda_county_geoid, "21009")
})

test_that("an input that is not PDA data is refused", {
  expect_error(
    transform_pda_counties(tibble::tibble(x = 1)),
    "missing")
})
