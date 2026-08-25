# Tests that values derived from the PDA reports agree with FEMA's own
# authoritative declaration and denial records. These translate the checks in
# vignettes/preliminary_damage_assessments_validation.Rmd into assertions so a
# parser change that degrades agreement fails the test suite instead of only
# changing numbers in a manually re-knit vignette.
#
# The standard here is indicative agreement, not perfection: the two sources
# genuinely disagree on a small share of records (and users are told to
# adjudicate those themselves), so each test asserts that at least 75% of
# comparable records agree.

agreement_threshold <- 0.75

## the joined dataset is built once and reused by every test in this file; the
## build reads the cached PDA extract and calls the OpenFEMA API
fema_agreement_env <- new.env(parent = emptyenv())

load_joined_pdas <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_offline(host = "www.fema.gov")

  cache_path <- testthat::test_path("..", "..", "data", "pdas.csv")
  testthat::skip_if_not(
    file.exists(cache_path),
    "no cached PDA dataset at data/pdas.csv")

  if (is.null(fema_agreement_env$pdas)) {
    fema_agreement_env$pdas <- suppressWarnings(suppressMessages(
      get_preliminary_damage_assessments(
        file_path = cache_path,
        use_cache = TRUE))) %>%
      ## PDA reports are only published from roughly 2007 onward; earlier FEMA
      ## records carry no PDA values to compare
      dplyr::filter(fema_decision_year > 2006)
  }
  fema_agreement_env$pdas
}

## the share of records on which two aligned vectors state the same value,
## among the records where both state one
agreement_share <- function(pda_values, fema_values) {
  comparable <- !is.na(pda_values) & !is.na(fema_values)
  if (sum(comparable) == 0) { return(NA_real_) }
  mean(pda_values[comparable] == fema_values[comparable])
}

test_that("the decision read from a report agrees with FEMA's recorded decision", {
  pdas <- load_joined_pdas()

  matched <- pdas %>% dplyr::filter(pda_matched)
  pda_decision_standardized <- dplyr::if_else(
    stringr::str_detect(matched$pda_decision, "approv"), "Approved", "Denied")

  expect_gte(
    agreement_share(pda_decision_standardized, matched$fema_decision),
    agreement_threshold)
})

test_that("a report's determination date does not follow FEMA's decision date", {
  pdas <- load_joined_pdas()

  matched <- pdas %>%
    dplyr::filter(
      pda_matched,
      !is.na(pda_date_determined), !is.na(fema_decision_date))

  expect_gte(
    mean(matched$pda_date_determined <= matched$fema_decision_date),
    agreement_threshold)
})

test_that("hazards read from a report overlap FEMA's own hazard record", {
  pdas <- load_joined_pdas()

  comparable <- pdas %>%
    dplyr::filter(!is.na(pda_hazards), !is.na(fema_hazards))
  shared <- shared_hazard_category_count(
    comparable$pda_hazards, comparable$fema_hazards)

  expect_gte(mean(shared > 0, na.rm = TRUE), agreement_threshold)
})

test_that("request flags read from denied reports agree with FEMA's denial records", {
  pdas <- load_joined_pdas()

  denied <- pdas %>% dplyr::filter(fema_decision == "Denied", pda_matched)

  expect_gte(
    agreement_share(denied$pda_pa_requested, denied$fema_pa_requested),
    agreement_threshold)
  ## FEMA records requests for the Individual Assistance umbrella under its
  ## Individuals and Households Program flag; `pda_ia_requested` is read more
  ## broadly (a component program alone counts), which explains some divergence
  expect_gte(
    agreement_share(denied$pda_ia_requested, denied$fema_ihp_requested),
    agreement_threshold)
})

test_that("a program FEMA declared was requested in the approved report", {
  ## the approved-side counterpart of the denial check above. FEMA's declared
  ## flags are not requests -- a program can be requested and not declared, or
  ## added by a later request -- so the comparison runs one way: a declared
  ## program should appear as requested in the report that preceded it.
  pdas <- load_joined_pdas()

  approved <- pdas %>% dplyr::filter(fema_decision == "Approved", pda_matched)

  pa_declared <- approved %>%
    dplyr::filter(fema_pa_declared, !is.na(pda_pa_requested))
  expect_gte(mean(pa_declared$pda_pa_requested), agreement_threshold)

  ihp_declared <- approved %>%
    dplyr::filter(fema_ihp_declared, !is.na(pda_ia_requested))
  expect_gte(mean(ihp_declared$pda_ia_requested), agreement_threshold)
})

test_that("the report-derived tribal flag agrees with FEMA's tribal request record", {
  pdas <- load_joined_pdas()

  matched <- pdas %>% dplyr::filter(pda_matched)

  expect_gte(
    agreement_share(
      as.logical(matched$pda_tribal_flag), matched$fema_tribal_request),
    agreement_threshold)

  ## every disagreement is written onto the record itself
  mismatched <- matched %>%
    dplyr::filter(
      !is.na(pda_tribal_flag), !is.na(fema_tribal_request),
      as.logical(pda_tribal_flag) != fema_tribal_request)
  expect_true(all(stringr::str_detect(
    mismatched$pda_warnings, "tribal government")))
})
