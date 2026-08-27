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

## A minimal stand-in for `fetch_declaration_areas()`: one row per
## declaration-area, with the columns the transformation reads.
areas_fixture <- function(disaster_number, fips_state_code, fips_county_code,
                          designated_area) {
  tibble::tibble(
    disaster_number = disaster_number,
    fips_state_code = fips_state_code,
    fips_county_code = fips_county_code,
    designated_area = designated_area,
    ih_program_declared = TRUE,
    ia_program_declared = FALSE,
    pa_program_declared = TRUE,
    hm_program_declared = TRUE)
}

## The joined output of `get_preliminary_damage_assessments()` keys its county
## detail on FEMA's disaster number, so a record that should reach the
## designated areas has to carry one.
pda_fixture_with_number <- function(countywide, disaster_number,
                                    state_name = "Kentucky") {
  pda_fixture(countywide, state_name = state_name) %>%
    dplyr::mutate(fema_disaster_number = disaster_number)
}

## `transform_pda_counties()` now reports how many records carry no county
## detail through `message()`; the tests are not checking that here.
transformed <- function(...) {
  suppressWarnings(suppressMessages(transform_pda_counties(...)))
}

test_that("a county listing becomes one row per county, with FIPS codes", {
  counties <- transformed(pda_fixture(
    "Barren County 14.84 Bullitt County 8.56 and Butler County 87.53."))

  expect_equal(nrow(counties), 3)
  expect_equal(
    counties$pda_county_name,
    c("Barren County", "Bullitt County", "Butler County"))
  expect_equal(counties$pda_county_geoid, c("21009", "21029", "21031"))
  expect_equal(counties$pda_pa_per_capita_impact_county, c(14.84, 8.56, 87.53))
  expect_equal(unique(counties$pda_geography_type), "county")
})

test_that("the sentence some reports append to the listing is not read as a county", {
  counties <- transformed(pda_fixture(
    stringr::str_c(
      "Barren County 14.84 and Butler County 0. ",
      "Joint PDAs have not been completed in the counties with a 0 per capita.")))

  expect_equal(nrow(counties), 2)
  expect_equal(counties$pda_county_name_reported, c("Barren County", "Butler County"))
})

test_that("a value of zero is preserved and flagged", {
  counties <- transformed(pda_fixture(
    "Barren County 14.84 and Butler County 0.00."))

  expect_equal(counties$pda_pa_per_capita_impact_county, c(14.84, 0))
  expect_true(is.na(counties$pda_warnings[1]))
  expect_match(counties$pda_warnings[2], "per capita impact of 0")
  expect_match(counties$pda_warnings[2], "preserved as printed")
})

test_that("a county listed twice returns two rows, both flagged", {
  counties <- transformed(pda_fixture(
    stringr::str_c(
      "First Incident - Barren County 6.34 and Butler County 12.54. ",
      "Second Incident - Barren County 2.28 and Butler County 0.55")))

  barren <- dplyr::filter(counties, pda_county_name == "Barren County")
  expect_equal(nrow(barren), 2)
  expect_equal(barren$pda_pa_per_capita_impact_county, c(6.34, 2.28))
  expect_true(all(stringr::str_detect(barren$pda_warnings, "listed more than once")))
})

test_that("warnings already on the record are kept alongside the new ones", {
  counties <- transformed(pda_fixture(
    "Butler County 0.00", warnings = "an existing problem"))

  expect_match(counties$pda_warnings, "an existing problem")
  expect_match(counties$pda_warnings, "per capita impact of 0")
})

test_that("a listing that ran past the end of its section is flagged", {
  counties <- transformed(pda_fixture(
    stringr::str_c(
      "Barren County 14.84 Primary Impact Damage to utilities ",
      "Total Public Assistance cost estimate 39614"),
    state_name = "West Virginia"))

  bled <- dplyr::filter(
    counties, stringr::str_detect(dplyr::coalesce(pda_warnings, ""), "ran past the end"))
  expect_equal(nrow(bled), 1)
  expect_equal(bled$pda_pa_per_capita_impact_county, 39614)
})

test_that("places that are not counties are typed and left without a FIPS code", {
  counties <- transformed(pda_fixture(
    c("Lower Kuskokwim REAA 12.00", "the Gila River Indian Community 43.00"),
    state_name = c("Alaska", "Arizona")))

  expect_equal(counties$pda_geography_type, c("education area", "tribal entity"))
  expect_true(all(is.na(counties$pda_county_geoid)))
  ## the leading "the" belongs to the sentence, not to the name
  expect_equal(counties$pda_county_name[2], "Gila River Indian Community")
})

test_that("the spellings FEMA and the Census disagree on still match", {
  counties <- transformed(pda_fixture(
    c("Prince George's County 4.00", "Orocovis Municipality 9.00",
      "Blount 3.00", "City and Borough of Juneau 20.00",
      "Saint Clair County 7.00"),
    state_name = c("Maryland", "Puerto Rico", "Alabama", "Alaska", "Missouri")))

  expect_equal(
    counties$pda_county_geoid,
    c("24033", "72107", "01009", "02110", "29185"))
  expect_equal(
    counties$pda_geography_type,
    c("county", "municipio", "county", "borough", "county"))
})

test_that("request-level columns with a county-level counterpart are dropped", {
  input <- pda_fixture_with_number("Barren County 14.84", "4001") %>%
    dplyr::mutate(
      fema_ihp_declared = TRUE, fema_ia_declared = FALSE,
      fema_pa_declared = TRUE, fema_hm_declared = TRUE)

  counties <- transformed(
    input,
    declaration_areas = areas_fixture("4001", "21", "009", "Barren"))

  expect_false("pda_pa_per_capita_impact_countywide" %in% names(counties))
  expect_false(any(
    c("fema_ihp_declared", "fema_ia_declared", "fema_pa_declared",
      "fema_hm_declared") %in% names(counties)))
  expect_true("pda_pa_per_capita_impact_county" %in% names(counties))
  expect_true("fema_pa_declared_county" %in% names(counties))
})

test_that("a record with no county listing is kept, with the county columns empty", {
  input <- pda_fixture(c("Barren County 14.84", NA_character_))

  counties <- transformed(input)

  expect_equal(nrow(counties), 2)
  empty <- dplyr::filter(counties, is.na(pda_county_name))
  expect_equal(nrow(empty), 1)
  expect_true(is.na(empty$pda_county_name))
  expect_true(is.na(empty$pda_county_geoid))
  expect_true(is.na(empty$pda_geography_type))
  expect_true(is.na(empty$pda_county_name_reported))
  expect_false(empty$pda_county_indicator)
  expect_false(empty$fema_county_indicator)
})

test_that("an approval with designated areas and no listing gets a row per county", {
  counties <- transformed(
    pda_fixture_with_number(NA_character_, "4001"),
    declaration_areas = areas_fixture(
      "4001", "21", c("009", "029"), c("Barren", "Bullitt")))

  expect_equal(nrow(counties), 2)
  expect_equal(counties$pda_county_geoid, c("21009", "21029"))
  expect_equal(counties$pda_county_name, c("Barren County", "Bullitt County"))
  expect_true(all(is.na(counties$pda_county_name_reported)))
  expect_true(all(is.na(counties$pda_pa_per_capita_impact_county)))
  expect_equal(counties$pda_county_indicator, c(FALSE, FALSE))
  expect_equal(counties$fema_county_indicator, c(TRUE, TRUE))
  expect_equal(counties$fema_statewide_request, c(FALSE, FALSE))
  expect_equal(counties$fema_pa_declared_county, c(TRUE, TRUE))
  expect_equal(counties$fema_ia_declared_county, c(FALSE, FALSE))
})

test_that("a county named by both sources is a single row carrying both indicators", {
  counties <- transformed(
    pda_fixture_with_number("Barren County 14.84", "4001"),
    declaration_areas = areas_fixture("4001", "21", "009", "Barren"))

  expect_equal(nrow(counties), 1)
  expect_equal(counties$pda_county_geoid, "21009")
  expect_true(counties$pda_county_indicator)
  expect_true(counties$fema_county_indicator)
  expect_equal(counties$pda_pa_per_capita_impact_county, 14.84)
  ## a county in both sources is not a county the report listed twice
  expect_true(is.na(counties$pda_warnings))
})

test_that("a county FEMA designated but the listing omitted becomes its own row", {
  counties <- transformed(
    pda_fixture_with_number("Barren County 14.84", "4001"),
    declaration_areas = areas_fixture(
      "4001", "21", c("009", "029"), c("Barren", "Bullitt")))

  expect_equal(nrow(counties), 2)
  barren <- dplyr::filter(counties, pda_county_geoid == "21009")
  bullitt <- dplyr::filter(counties, pda_county_geoid == "21029")
  expect_equal(c(barren$pda_county_indicator, barren$fema_county_indicator), c(TRUE, TRUE))
  expect_equal(c(bullitt$pda_county_indicator, bullitt$fema_county_indicator), c(FALSE, TRUE))
  expect_equal(bullitt$pda_county_name, "Bullitt County")
  expect_equal(bullitt$pda_geography_type, "county")
  expect_true(is.na(bullitt$pda_pa_per_capita_impact_county))
})

test_that("a statewide designation expands to every current county in the state", {
  counties <- transformed(
    pda_fixture_with_number(NA_character_, "4001", state_name = "Connecticut"),
    declaration_areas = areas_fixture("4001", "09", "000", "Statewide"))

  ## Connecticut's nine planning regions, not the eight counties they replaced
  expect_equal(nrow(counties), 9)
  expect_true(all(counties$fema_statewide_request))
  expect_true(all(counties$fema_county_indicator))
  expect_false(any(counties$pda_county_indicator))
  expect_true(all(stringr::str_starts(counties$pda_county_geoid, "09")))
  expect_true(all(
    as.integer(stringr::str_sub(counties$pda_county_geoid, 3)) >= 110))
})

test_that("a designated area that is not a county keeps its name and no county code", {
  counties <- transformed(
    pda_fixture_with_number(NA_character_, "4001", state_name = "North Dakota"),
    declaration_areas = areas_fixture(
      "4001", "38", "000", "Turtle Mountain Indian Reservation"))

  expect_equal(nrow(counties), 1)
  expect_true(is.na(counties$pda_county_geoid))
  expect_false(counties$fema_statewide_request)
  expect_true(counties$fema_county_indicator)
  expect_equal(counties$fema_designated_area, "Turtle Mountain Indian Reservation")
})

test_that("a denial with no matched report keeps a single empty row", {
  counties <- transformed(
    pda_fixture_with_number(NA_character_, NA_character_),
    declaration_areas = areas_fixture("4001", "21", "009", "Barren"))

  expect_equal(nrow(counties), 1)
  expect_true(is.na(counties$pda_county_geoid))
  expect_false(counties$pda_county_indicator)
  expect_false(counties$fema_county_indicator)
  expect_false(counties$fema_statewide_request)
})

test_that("the joined output's FEMA state name is used where present", {
  input <- tibble::tibble(
    fema_state_name = "Kentucky",
    pda_path = "report_1.pdf",
    pda_pa_per_capita_impact_countywide = "Barren County 14.84",
    pda_warnings = NA_character_)

  counties <- transformed(input)

  expect_true("pda_county_geoid" %in% names(counties))
  expect_true("pda_geography_type" %in% names(counties))
  expect_equal(counties$pda_county_geoid, "21009")
})

test_that("the request identifier is repeated down a record's county rows", {
  input <- pda_fixture_with_number("Barren County 14.84", "4001") %>%
    dplyr::mutate(fema_declaration_request_number = "22094")

  counties <- transformed(
    input,
    declaration_areas = areas_fixture(
      "4001", "21", c("009", "029", "031"), c("Barren", "Bullitt", "Butler")))

  expect_equal(nrow(counties), 3)
  expect_equal(counties$fema_declaration_request_number, rep("22094", 3))
  ## the request number and the county code together identify a row
  expect_equal(
    nrow(dplyr::distinct(
      counties, fema_declaration_request_number, pda_county_geoid)),
    3)
})

test_that("an input that is not PDA data is refused", {
  expect_error(
    transform_pda_counties(tibble::tibble(x = 1)),
    "missing")
})
