# Tests for match_denied_pdas_to_denials() in
# get_preliminary_damage_assessments.R

## Minimal stand-ins for the two sides of the match. The hand-checked link
## table references reports absent from these fixtures, which raises its
## review warning; the calls are wrapped in suppressWarnings for that reason.
denied_pda_fixture <- function(...) {
  tibble::tibble(
    decision = "Denied",
    ...)
}

denial_fixture <- function(...) {
  tibble::tibble(decision = "Denied", ...) %>%
    ## FEMA's own request number identifies a denial, so the fixture hands out
    ## one per row rather than building a key out of the other fields
    dplyr::mutate(
      declaration_request_number = as.character(24000 + dplyr::row_number()))
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
