# Tests for match_denied_pdas_to_denials() in
# get_preliminary_damage_assessments.R

## Minimal stand-ins for the two sides of the match. The hand-checked link
## table references reports absent from these fixtures, which raises its
## review warning; the calls are wrapped in suppressWarnings for that reason.
## The filing date and the tribe's states are unknown unless a test supplies
## them, as they are for most reports.
denied_pda_fixture <- function(...) {
  pdas <- tibble::tibble(decision = "Denied", ...)
  if (!"request_date_stated" %in% names(pdas)) {
    pdas <- dplyr::mutate(pdas, request_date_stated = as.Date(NA)) }
  if (!"tribal_state_names" %in% names(pdas)) {
    pdas <- dplyr::mutate(pdas, tribal_state_names = NA_character_) }
  pdas
}

denial_fixture <- function(...) {
  denials <- tibble::tibble(decision = "Denied", ...) %>%
    ## FEMA's own request number identifies a denial, so the fixture hands out
    ## one per row rather than building a key out of the other fields
    dplyr::mutate(
      declaration_request_number = as.character(24000 + dplyr::row_number()))
  if (!"declaration_request_date" %in% names(denials)) {
    denials <- dplyr::mutate(denials, declaration_request_date = as.Date(NA)) }
  denials
}

test_that("a unique same-day denial is matched exactly", {
  pdas <- denied_pda_fixture(
    path = "ky.pdf",
    event_title = "Kentucky Severe Storms",
    state_name = "Kentucky",
    event_date_determined = as.Date("2021-05-01"),
    hazards = "severe storm")
  denials <- denial_fixture(
    state_name = "Kentucky",
    request_status_date = as.Date("2021-05-01"),
    declaration_title = "Severe Storms",
    denial_hazards = "severe storm")

  matched <- suppressWarnings(match_denied_pdas_to_denials(pdas, denials))

  expect_equal(matched$declaration_request_number, denials$declaration_request_number)
  expect_match(matched$match_quality, "^exact")
})

test_that("two same-day denials in one state are separated by hazard", {
  pdas <- denied_pda_fixture(
    path = "tx.pdf",
    event_title = "Texas Wildfire",
    state_name = "Texas",
    event_date_determined = as.Date("2021-05-01"),
    hazards = "wildfire")
  denials <- denial_fixture(
    state_name = "Texas",
    request_status_date = as.Date("2021-05-01"),
    declaration_title = c("Flooding", "Wildfire"),
    denial_hazards = c("flooding", "wildfire"))

  matched <- suppressWarnings(match_denied_pdas_to_denials(pdas, denials))

  expect_equal(matched$declaration_request_number, denials$declaration_request_number[2])
})

test_that("two same-day denials that hazard cannot separate stay unmatched", {
  pdas <- denied_pda_fixture(
    path = "tx.pdf",
    event_title = "Texas Flooding",
    state_name = "Texas",
    event_date_determined = as.Date("2021-05-01"),
    hazards = "flooding")
  denials <- denial_fixture(
    state_name = "Texas",
    request_status_date = as.Date("2021-05-01"),
    declaration_title = c("Flooding North", "Flooding South"),
    denial_hazards = c("flooding", "flooding"))

  matched <- suppressWarnings(match_denied_pdas_to_denials(pdas, denials))

  expect_equal(nrow(matched), 1)
  expect_true(is.na(matched$declaration_request_number))
  expect_true(is.na(matched$match_quality))
})

test_that("with no same-day denial, the nearest same-hazard one within a week is used", {
  pdas <- denied_pda_fixture(
    path = "mo.pdf",
    event_title = "Missouri Flooding",
    state_name = "Missouri",
    event_date_determined = as.Date("2021-05-04"),
    hazards = "flooding")
  denials <- denial_fixture(
    state_name = "Missouri",
    request_status_date = as.Date("2021-05-01"),
    declaration_title = "Flooding",
    denial_hazards = "flooding")

  matched <- suppressWarnings(match_denied_pdas_to_denials(pdas, denials))

  expect_equal(matched$declaration_request_number, denials$declaration_request_number)
  expect_match(matched$match_quality, "^approximate")
  expect_match(matched$match_quality, "3 day\\(s\\) earlier")
})

test_that("a nearby denial of a different hazard is refused", {
  pdas <- denied_pda_fixture(
    path = "mo.pdf",
    event_title = "Missouri Flooding",
    state_name = "Missouri",
    event_date_determined = as.Date("2021-05-04"),
    hazards = "flooding")
  denials <- denial_fixture(
    state_name = "Missouri",
    request_status_date = as.Date("2021-05-01"),
    declaration_title = "Wildfire",
    denial_hazards = "wildfire")

  matched <- suppressWarnings(match_denied_pdas_to_denials(pdas, denials))

  expect_true(is.na(matched$declaration_request_number))
})

test_that("a report naming no state is matched on the date alone when unambiguous", {
  ## a tribal report names the tribe rather than a state, so the state-keyed
  ## passes never see it; the date-only pass may claim the one unclaimed
  ## denial in the country on that date, given hazard agreement
  pdas <- denied_pda_fixture(
    path = "tribe.pdf",
    event_title = "Example Tribe - Severe Storms",
    state_name = NA_character_,
    event_date_determined = as.Date("2021-06-01"),
    hazards = "severe storm")
  denials <- denial_fixture(
    state_name = c("South Dakota", "Ohio"),
    request_status_date = as.Date(c("2021-06-01", "2021-07-01")),
    declaration_title = c("Severe Storms", "Flooding"),
    denial_hazards = c("severe storm", "flooding"))

  matched <- suppressWarnings(match_denied_pdas_to_denials(pdas, denials))

  expect_equal(matched$declaration_request_number, denials$declaration_request_number[1])
  expect_match(matched$match_quality, "without the state key")
})

test_that("the date-only pass declines when two denials share the date", {
  pdas <- denied_pda_fixture(
    path = "tribe.pdf",
    event_title = "Example Tribe - Severe Storms",
    state_name = NA_character_,
    event_date_determined = as.Date("2021-06-01"),
    hazards = "severe storm")
  denials <- denial_fixture(
    state_name = c("South Dakota", "Ohio"),
    request_status_date = as.Date(c("2021-06-01", "2021-06-01")),
    declaration_title = c("Severe Storms", "Severe Storms and Flooding"),
    denial_hazards = c("severe storm", "flooding; severe storm"))

  matched <- suppressWarnings(match_denied_pdas_to_denials(pdas, denials))

  expect_true(is.na(matched$declaration_request_number))
})

test_that("an appeal FEMA never recorded is matched on the filing date", {
  ## FEMA's record carries the original denial's date; the report is the
  ## appeal, decided six weeks later, but states the same filing date
  pdas <- denied_pda_fixture(
    path = "nj.pdf",
    event_title = "Severe Storms and Flooding",
    state_name = "New Jersey",
    event_date_determined = as.Date("2015-12-21"),
    hazards = "flooding; severe storm",
    request_date_stated = as.Date("2015-10-29"))
  denials <- denial_fixture(
    state_name = "New Jersey",
    request_status_date = as.Date("2015-11-10"),
    declaration_request_date = as.Date("2015-10-29"),
    declaration_title = "NJ Severe Coastal Storm",
    denial_hazards = "coastal storm")

  matched <- suppressWarnings(match_denied_pdas_to_denials(pdas, denials))

  expect_equal(matched$declaration_request_number, denials$declaration_request_number)
  expect_match(matched$match_quality, "filing date")
  expect_false(any(c("request_date_stated", "tribal_state_names") %in% names(matched)))
})

test_that("the filing-date pass refuses a denial filed the same day in another state", {
  pdas <- denied_pda_fixture(
    path = "nj.pdf",
    event_title = "Severe Storms and Flooding",
    state_name = "New Jersey",
    event_date_determined = as.Date("2015-12-21"),
    hazards = "flooding; severe storm",
    request_date_stated = as.Date("2015-10-29"))
  denials <- denial_fixture(
    state_name = "New York",
    request_status_date = as.Date("2015-11-10"),
    declaration_request_date = as.Date("2015-10-29"),
    declaration_title = "Severe Storms and Flooding",
    denial_hazards = "flooding; severe storm")

  matched <- suppressWarnings(match_denied_pdas_to_denials(pdas, denials))

  expect_true(is.na(matched$declaration_request_number))
})

test_that("the filing-date pass refuses a denial unrelated to the report", {
  ## a report that misprints its filing date can land on another request of
  ## the same state; sharing neither hazard nor a title word, it is refused
  pdas <- denied_pda_fixture(
    path = "ak.pdf",
    event_title = "Building Collapse",
    state_name = "Alaska",
    event_date_determined = as.Date("2024-03-15"),
    hazards = NA_character_,
    request_date_stated = as.Date("2023-09-05"))
  denials <- denial_fixture(
    state_name = "Alaska",
    request_status_date = as.Date("2023-09-26"),
    declaration_request_date = as.Date("2023-09-05"),
    declaration_title = "AK_Mendenhall Glacier Dam Burst",
    denial_hazards = NA_character_)

  matched <- suppressWarnings(match_denied_pdas_to_denials(pdas, denials))

  expect_true(is.na(matched$declaration_request_number))
})

test_that("a tribal report is matched on the filing date in the tribe's state", {
  pdas <- denied_pda_fixture(
    path = "leech_lake.pdf",
    event_title = "Leech Lake Band of Ojibwe - Building Fire",
    state_name = NA_character_,
    event_date_determined = as.Date("2026-07-02"),
    hazards = "wildfire",
    request_date_stated = as.Date("2025-11-30"),
    tribal_state_names = "Minnesota")
  denials <- denial_fixture(
    state_name = c("Minnesota", "Alaska"),
    request_status_date = as.Date(c("2026-07-02", "2026-07-02")),
    declaration_request_date = as.Date(c("2025-11-30", "2025-11-30")),
    declaration_title = c(
      "R5 - Leech Lake Band of Ojibwe - Sep 1 2025 - Fire",
      "Tribal Fire"),
    denial_hazards = c("wildfire", "wildfire"))

  matched <- suppressWarnings(match_denied_pdas_to_denials(pdas, denials))

  expect_equal(matched$declaration_request_number, denials$declaration_request_number[1])
})

test_that("the date-only pass looks only in the tribe's states when they are known", {
  ## several denials share the date nationally, but only one lies in a state
  ## the tribe's lands lie in
  pdas <- denied_pda_fixture(
    path = "chickasaw.pdf",
    event_title = "Chickasaw Nation - Severe Winter Storm",
    state_name = NA_character_,
    event_date_determined = as.Date("2026-05-31"),
    hazards = "winter storm",
    tribal_state_names = "Oklahoma; Texas")
  denials <- denial_fixture(
    state_name = c("Montana", "Oklahoma"),
    request_status_date = as.Date(c("2026-05-31", "2026-05-31")),
    declaration_title = c("R8 MT Winds and Winter Storm, December 2025", "Severe Winter Storm"),
    denial_hazards = c("severe storm; winter storm", "winter storm"))

  matched <- suppressWarnings(match_denied_pdas_to_denials(pdas, denials))

  expect_equal(matched$declaration_request_number, denials$declaration_request_number[2])
  expect_match(matched$match_quality, "states the tribe's lands lie in")
})
