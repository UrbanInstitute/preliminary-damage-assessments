# Tests for get_preliminary_damage_assessments.R

test_that("the event title keeps the event and drops everything else", {
  reports <- tibble::tibble(
    text = c(
      "Colorado wildfire report text",
      "North Carolina report text",
      "Kentucky report text"),
    event_title = c(
      "Preliminary Damage Assessment Report - Colorado - Marshall Fire",
      "Flooding in North Carolina",
      "Commonwealth of Kentucky Severe Storms and Flooding FEMA-4595-DR Declared July 29, 2022"))

  cleaned <- add_pda_derived_columns(reports)

  expect_equal(cleaned$event_title[1], "Marshall Fire")
  ## a state name inside the description is part of the description
  expect_equal(cleaned$event_title[2], "Flooding in North Carolina")
  expect_equal(cleaned$event_title[3], "Severe Storms and Flooding")
  expect_equal(cleaned$state_name, c("Colorado", "North Carolina", "Kentucky"))
})

test_that("hazard categories are consistent across differing source wordings", {
  reports <- tibble::tibble(
    text = c("Vermont report text", "Louisiana report text", "Alaska report text"),
    event_title = c(
      "Severe Storms, Floods, Landslides, and Mudslides",
      "Hurricane Ida",
      "Structure Fire"))

  cleaned <- add_pda_derived_columns(reports)

  ## "Floods" and "Flooding" both become "flooding"; mudslides and landslides
  ## are one category rather than two names for the same thing
  expect_equal(cleaned$hazards[1], "flooding; severe storm; landslide")
  expect_equal(cleaned$hazards[2], "hurricane")
  expect_equal(cleaned$hazards[3], "wildfire")
})

test_that("shared hazard counts distinguish partial from complete disagreement", {
  expect_equal(
    shared_hazard_category_count(
      c("flooding; severe storm", "flooding", "wildfire"),
      c("flooding; severe storm", "wildfire", NA_character_)),
    c(2L, 0L, NA_integer_))
})

test_that("the hand-checked denial links are well formed", {
  links <- manual_pda_denial_links()

  expect_named(links, c("pda_file", "declaration_request_number", "note"))
  ## a report may be linked once and a denial claimed once, or the table
  ## itself would create the ambiguity it exists to resolve
  expect_equal(anyDuplicated(links$pda_file), 0)
  expect_equal(anyDuplicated(links$declaration_request_number), 0)
  ## a link is recorded as FEMA's own declaration request number
  expect_true(all(stringr::str_detect(links$declaration_request_number, "^[0-9]+$")))
  expect_true(all(nchar(links$note) > 0))
})

test_that("shared title words ignore the words common to every tribal name", {
  ## "Tribe" and "Band" appear in most tribal names, so counting them would make
  ## any two tribal records look related
  expect_equal(
    shared_title_word_count(
      "Spokane Tribe - Cayuse Mountain Wildfire",
      "Cayuse Mountain Fire"),
    2L)
  expect_equal(
    shared_title_word_count("Oglala Sioux Tribe", "Cheyenne River Sioux Band"),
    1L)
  expect_equal(shared_title_word_count("Navajo Nation", "Tribe"), NA_integer_)
})

test_that("FEMA's abbreviated tropical storm names are recognised", {
  ## FEMA writes "TS Cristobal" in its own turndown records
  expect_equal(extract_hazard_categories("TS Cristobal"), "tropical storm")
  expect_equal(
    extract_hazard_categories("Tropical Storm Cristobal"), "tropical storm")
  ## the abbreviation must be a word on its own, not a fragment of another
  expect_true(is.na(extract_hazard_categories("Tsunami warning aftermath")) |
    !stringr::str_detect(
      extract_hazard_categories("Tsunami warning aftermath"), "tropical"))
})

test_that("misspelled and compound storm titles are recognised", {
  ## one report title reads "Texas Tropical Strom Erin"
  expect_equal(
    extract_hazard_categories("Texas Tropical Strom Erin"), "tropical storm")
  expect_equal(
    extract_hazard_categories("Washington - Severe Windstorm"), "severe storm")
})

test_that("tropical and coastal storm count as the same event when compared", {
  ## the same landfalling system is a tropical storm to the state and a coastal
  ## storm to FEMA; the two labels are kept apart but agree for matching
  expect_equal(shared_hazard_category_count("tropical storm", "coastal storm"), 1L)
  expect_equal(
    shared_hazard_category_count("flooding; tropical storm", "flooding; coastal storm"),
    2L)
  ## unrelated hazards still disagree
  expect_equal(shared_hazard_category_count("wildfire", "coastal storm"), 0L)
})

test_that("a missing statewide indicator is filled from its fiscal year's reports", {
  ## the indicator is national and changes on 1 October, so reports from
  ## November 2019 and March 2020 belong to the same fiscal year as each other.
  ## The fourth report is a tribal request, which is assessed against the same
  ## national figure and so is filled in like any other.
  reports <- tibble::tibble(
    text = rep("Statewide Per Capita Impact Indicator for FY20", 4),
    event_date_determined = as.Date(c(
      "2019-11-05", "2020-03-10", "2020-06-01", "2020-05-05")),
    event_native_flag = c(0, 0, 0, 1),
    pa_per_capita_impact_indicator_statewide = c(1.53, 1.53, NA, NA))

  filled <- suppressMessages(impute_per_capita_indicators(reports))

  expect_equal(
    filled$pa_per_capita_impact_indicator_statewide, c(1.53, 1.53, 1.53, 1.53))
  expect_equal(
    filled$pa_per_capita_impact_indicator_statewide_source,
    c("reported", "reported", "imputed", "imputed"))
})

test_that("a stated value outside the published range is replaced", {
  ## two reports state 1.53 and one states 3.11, which is outside the published
  ## range and is the countywide threshold printed in the statewide row
  reports <- tibble::tibble(
    text = rep("Statewide Per Capita Impact Indicator for FY20", 4),
    event_date_determined = as.Date(rep("2020-05-05", 4)),
    event_native_flag = rep(0, 4),
    pa_per_capita_impact_indicator_statewide = c(1.53, 1.53, 3.11, NA))

  filled <- suppressMessages(impute_per_capita_indicators(reports))

  ## the out-of-range value does not set the year's value ...
  expect_equal(filled$pa_per_capita_impact_indicator_statewide[4], 1.53)
  ## ... and is itself replaced, which is said so in the source column
  expect_equal(filled$pa_per_capita_impact_indicator_statewide[3], 1.53)
  expect_equal(
    filled$pa_per_capita_impact_indicator_statewide_source,
    c("reported", "reported",
      "imputed, replacing a stated value outside the published range", "imputed"))
})

test_that("the countywide indicator is filled and corrected like the statewide one", {
  ## the countywide threshold is national and yearly in the same way, so a
  ## missing value and a value outside its own published range are both
  ## replaced with the value the rest of the fiscal year states
  reports <- tibble::tibble(
    text = rep("Statewide Per Capita Impact Indicator for FY20", 3),
    event_date_determined = as.Date(rep("2020-05-05", 3)),
    event_native_flag = rep(0, 3),
    pa_per_capita_impact_indicator_statewide = c(1.53, 1.53, 1.53),
    pa_per_capita_impact_indicator_countywide = c(3.84, 1.00, NA),
    pa_per_capita_impact_statewide = c(2.10, 999, 1.00))

  filled <- suppressMessages(impute_per_capita_indicators(reports))

  expect_equal(
    filled$pa_per_capita_impact_indicator_countywide, c(3.84, 3.84, 3.84))
  expect_equal(
    filled$pa_per_capita_impact_indicator_countywide_source,
    c("reported",
      "imputed, replacing a stated value outside the published range",
      "imputed"))
  ## the measured per capita impacts are not thresholds and are left alone
  expect_equal(filled$pa_per_capita_impact_statewide, c(2.10, 999, 1.00))
})

test_that("a countywide fiscal year with no reported value is left empty", {
  reports <- tibble::tibble(
    text = rep("Statewide Per Capita Impact Indicator for FY20", 2),
    event_date_determined = as.Date(c("2020-05-05", "2020-06-06")),
    event_native_flag = c(0, 0),
    pa_per_capita_impact_indicator_statewide = c(1.53, 1.53),
    pa_per_capita_impact_indicator_countywide = c(NA_real_, NA_real_))

  filled <- suppressMessages(impute_per_capita_indicators(reports))

  expect_true(all(is.na(filled$pa_per_capita_impact_indicator_countywide)))
  expect_true(all(is.na(filled$pa_per_capita_impact_indicator_countywide_source)))
})

test_that("an October-to-December fill is marked as possibly a year too recent", {
  reports <- tibble::tibble(
    text = rep("Statewide Per Capita Impact Indicator for FY24", 3),
    event_date_determined = as.Date(c("2023-10-11", "2024-03-01", "2024-02-01")),
    event_native_flag = rep(0, 3),
    pa_per_capita_impact_indicator_statewide = c(NA, NA, 1.84))

  filled <- suppressMessages(impute_per_capita_indicators(reports))

  expect_equal(
    filled$pa_per_capita_impact_indicator_statewide_source,
    c("imputed, and may be one fiscal year too recent", "imputed", "reported"))
})

test_that("a fiscal year with no reported value is left empty", {
  reports <- tibble::tibble(
    text = rep("Statewide Per Capita Impact Indicator for FY20", 2),
    event_date_determined = as.Date(c("2020-05-05", "2020-06-06")),
    event_native_flag = c(0, 0),
    pa_per_capita_impact_indicator_statewide = c(NA_real_, NA_real_))

  filled <- suppressMessages(impute_per_capita_indicators(reports))

  expect_true(all(is.na(filled$pa_per_capita_impact_indicator_statewide)))
  expect_true(all(is.na(filled$pa_per_capita_impact_indicator_statewide_source)))
})

test_that("an Alaska Native village is recognised as a tribal requester", {
  ## the pattern the extraction applies to the report title
  is_tribal <- function(title) {
    stringr::str_detect(
      title,
      stringr::regex(
        stringr::str_c(
          "\\b(Native|Tribe|Tribes|Tribal|Indians|Nation|Band|Pueblo|",
          "Rancheria|Reservation|Villages?)\\b|",
          "\\b(Traditional|IRA) Council\\b"),
        ignore_case = TRUE)) }

  ## names an Alaska Native village with none of the older keywords
  expect_true(is_tribal("Newtok Village - Flooding, Persistent Erosion"))
  expect_true(is_tribal("Native Village of Kivalina - Severe Storms"))
  expect_true(is_tribal("Chevak Traditional Council - Building Fire"))
  ## and does not fire on an ordinary state request
  expect_false(is_tribal("Severe Storms, Straight-line Winds, and Flooding"))
  expect_false(is_tribal("National Weather Service reported flooding"))
})

test_that("the narrative settles a report that contradicts itself", {
  ## the first report says Public Assistance was not requested and then reports
  ## a cost for it; the second says the same but reports nothing, so there is no
  ## contradiction to settle
  reports <- tibble::tibble(
    path = c("a.pdf", "b.pdf"),
    text = rep(paste(
      "The Governor requested a declaration for Public Assistance for four",
      "counties and Hazard Mitigation statewide."), 2),
    ia_requested = c(0, 0),
    pa_requested = c(0, 0),
    pa_cost_estimate_total = c(1e6, NA))

  settled <- suppressMessages(resolve_requested_flags(reports))

  expect_equal(settled$pa_requested, c(1, 0))
  expect_equal(settled$requested_from_narrative, c("pa", NA))
})

test_that("a component-program request reads as a request for Individual Assistance", {
  ## Individual Assistance is read broadly: a request for any of its component
  ## programs -- Crisis Counseling, Disaster Unemployment Assistance, Disaster
  ## Legal Services, Disaster Case Management -- counts, whether or not the
  ## sentence names Individual Assistance itself. This is deliberately wider
  ## than FEMA's ihProgramRequested field, which records only the Individuals
  ## and Households Program.
  component <- c(
    "requested a declaration for Crisis Counseling under the Individual Assistance program for three counties.",
    "requested a declaration for the Crisis Counseling Program and Disaster Unemployment Assistance under the Individual Assistance program for one county.",
    "requested a declaration for Disaster Unemployment Assistance for two counties.",
    "requested a declaration for Disaster Legal Services and Disaster Case Management statewide.")
  full <- c(
    "requested a declaration for Individual Assistance, including the Crisis Counseling Program, for two counties.",
    "requested a declaration for the Individuals and Households Program, Crisis Counseling, for one county.")

  settle <- function(sentences) {
    suppressMessages(resolve_requested_flags(tibble::tibble(
      path = seq_along(sentences), text = sentences,
      ia_requested = 0, pa_requested = 1,
      ia_residences_impacted = 10)))$ia_requested }

  expect_equal(settle(component), c(1, 1, 1, 1))
  expect_equal(settle(full), c(1, 1))
})

test_that("an SBA-loan-only request is not a request for Individual Assistance", {
  ## The 2025 Wisconsin "Lack of Snow" denial: the Governor requested only a
  ## Small Business Administration Economic Injury Loan, the summary carries no
  ## "Not requested" line for Individual Assistance, and the only non-dash
  ## Individual Assistance figures are a literal 0 and $0. The zeros are
  ## placeholders, not measurements, and the SBA sentence is a readable
  ## narrative that requests neither program.
  reports <- tibble::tibble(
    path = "wi.pdf",
    text = paste(
      "The Governor requested a declaration for a Small Business",
      "Administration (SBA) Economic Injury Loan for all 72 counties.",
      "Summary of Damage Assessment Information."),
    ia_requested = 1, pa_requested = 0,
    ia_residences_impacted = NA_real_,
    ia_ihp_cost_to_capacity_ratio = 0,
    ia_cost_estimate_total = 0)

  settled <- suppressMessages(resolve_requested_flags(reports))

  expect_equal(settled$ia_requested, 0)
  expect_equal(settled$requested_from_narrative, "ia")
})

test_that("a report with no narrative sentence keeps the summary's flag", {
  reports <- tibble::tibble(
    path = "a.pdf",
    text = "Summary of Damage Assessment Information. Individual Assistance - Not requested",
    ia_requested = 0, pa_requested = 1,
    ia_residences_impacted = 10)

  settled <- suppressMessages(resolve_requested_flags(reports))

  expect_equal(settled$ia_requested, 0)
  expect_true(is.na(settled$requested_from_narrative))
})

test_that("a program the narrative never names, with no values, is not requested", {
  ## FEMA-4585-DR (Alaska): the Governor requested Public Assistance and Hazard
  ## Mitigation only, the summary omits the "Not requested" wording, and every
  ## Individual Assistance field is a placeholder. The summary alone would read
  ## this as a request for Individual Assistance.
  reports <- tibble::tibble(
    path = "4585.pdf",
    text = paste(
      "The Governor requested Public Assistance program for five areas and",
      "Hazard Mitigation statewide. Summary of Damage Assessment Information.",
      "Individual Assistance Total Number of Residences Impacted: -"),
    ia_requested = 1, pa_requested = 1,
    ia_residences_impacted = NA_real_,
    pa_cost_estimate_total = 24047546)

  settled <- suppressMessages(resolve_requested_flags(reports))

  expect_equal(settled$ia_requested, 0)
  expect_equal(settled$pa_requested, 1)
  expect_equal(settled$requested_from_narrative, "ia")
})

test_that("a program the narrative names keeps its flag even with no values", {
  ## FEMA-4707-DR (Hoopa Valley Tribe): the narrative does request Individual
  ## Assistance, and the summary reports N/A throughout because no damage
  ## figures were collected. The flag stays at 1.
  reports <- tibble::tibble(
    path = "4707.pdf",
    text = paste(
      "The Chairman requested a declaration for the Individuals and Households",
      "program under the Individual Assistance, Public Assistance, including",
      "direct federal assistance and snow assistance, and Hazard Mitigation for",
      "the Hoopa Valley Tribe. Summary of Damage Assessment Information."),
    ia_requested = 1, pa_requested = 1,
    ia_residences_impacted = NA_real_,
    pa_cost_estimate_total = 649321)

  settled <- suppressMessages(resolve_requested_flags(reports))

  expect_equal(settled$ia_requested, 1)
  expect_true(is.na(settled$requested_from_narrative))
})

test_that("an unreadable narrative never sets a flag on its own", {
  ## A report whose request sentences name none of the four programs is not
  ## evidence either way, so the summary's flag stands.
  reports <- tibble::tibble(
    path = "denial.pdf",
    text = paste(
      "The Governor's request was denied on appeal.",
      "Summary of Damage Assessment Information."),
    ia_requested = 1, pa_requested = 1,
    ia_residences_impacted = NA_real_,
    pa_cost_estimate_total = NA_real_)

  settled <- suppressMessages(resolve_requested_flags(reports))

  expect_equal(settled$ia_requested, 1)
  expect_equal(settled$pa_requested, 1)
  expect_true(is.na(settled$requested_from_narrative))
})

test_that("one place has one name and one FIPS code however a source spells it", {
  spellings <- c(
    "Virgin Islands of the U.S.", "U.S. Virgin Islands", "Marshall Islands",
    "District of Columbia", "Texas")

  expect_equal(
    standardize_state_names(spellings),
    c("U.S. Virgin Islands", "U.S. Virgin Islands", "Marshall Islands",
      "District of Columbia", "Texas"))
  expect_equal(
    state_names_to_fips(spellings),
    c("78", "78", "68", "11", "48"))
  ## a name belonging to no state or territory yields nothing rather than a
  ## wrong code
  expect_true(is.na(standardize_state_names("Ontario")))
})

test_that("the freely associated states are recognized", {
  reference <- state_reference()

  expect_true(all(
    c("Federated States of Micronesia", "Marshall Islands", "Palau") %in%
      reference$state_name))
  ## the state name pattern is built from the same reference, so a report
  ## titled for one of them has its state read
  expect_equal(
    stringr::str_extract(
      "Federated States of Micronesia High Tides and Drought",
      state_name_pattern()),
    "Federated States of Micronesia")
})

test_that("a tribal report title yields the tribe and a state title yields none", {
  titles <- c(
    "Hoopa Valley Tribe – Severe Winter Storm",
    "Karuk Tribe - Wildfire",
    "Tohono O’odham Nation – Severe Storms and Flooding",
    "Marshall Fire",
    "Colorado – Severe Storms")

  expect_equal(
    extract_tribal_names(titles),
    c("Hoopa Valley Tribe", "Karuk Tribe", "Tohono O'odham Nation",
      NA_character_, NA_character_))
})

test_that("a tribe FEMA names two ways comes back under one name", {
  titles <- c(
    "Fort Peck Assiniboine & Sioux Tribes – Severe Storm",
    "Assiniboine and Sioux Tribes of the Fort Peck Indian Reservation - Severe Storm")

  extracted <- extract_tribal_names(titles)

  expect_equal(length(unique(extracted)), 1)
  expect_equal(
    unique(extracted),
    "Assiniboine and Sioux Tribes of the Fort Peck Indian Reservation")
})

test_that("generic words are dropped and state abbreviations spelled out", {
  ## the tribe's name and its area's name share only their identifying words
  expect_equal(tribal_name_tokens("Spokane Tribe")[[1]], "spokane")
  expect_equal(tribal_name_tokens("Spokane Reservation")[[1]], "spokane")
  expect_equal(
    tribal_name_tokens("Ponca (NE) Trust Land")[[1]],
    tribal_name_tokens("Ponca Tribe of Nebraska")[[1]])
  ## the reports spell out what the Census names abbreviate
  expect_equal(
    tribal_name_tokens("Saint Regis Mohawk Tribe")[[1]],
    tribal_name_tokens("St. Regis Mohawk Reservation")[[1]])
})

test_that("tribes are matched to the Census area they govern", {
  skip_if_offline()

  tribes <- c(
    "Cheyenne River Sioux Tribe",
    ## the area's name says nothing about the tribe, so it comes from the
    ## hand-checked list
    "Oglala Sioux Tribe",
    ## the only word this shares with any area is one many areas carry
    "Nonexistent Sioux Tribe")

  matched <- match_tribal_names_to_native_areas(tribes)
  areas <- sf::st_drop_geometry(tigris::native_areas(year = 2023, progress_bar = FALSE))

  expect_equal(
    areas$NAMELSAD[match(matched, areas$GEOID)],
    c("Cheyenne River Reservation", "Pine Ridge Reservation", NA_character_))
})

test_that("a title-case Public Assistance cost label is still read", {
  ## The reports for disasters 1736-1739 print "Total Public Assistance Cost
  ## Estimate:" where every other report prints "cost estimate"; both casings
  ## must yield the value.
  report_text <- paste0(
    "Preliminary Damage Assessment Report\n",
    "State of Missouri - Severe Storms and Flooding\n",
    "FEMA-1736-DR Declared November 13, 2007\n",
    "Summary of Damage Assessment Information: Public Assistance: ",
    "• Primary Impact: Cost for debris removal ",
    "• Total Public Assistance Cost Estimate: $28,931,081 ",
    "• Statewide per capita impact: $5.17 ",
    "• Statewide per capita impact indicator: $1.24 ",
    "• Countywide per capita impact: Adair County ($3.56) ",
    "• Countywide per capita impact indicator: $3.11 ",
    "The Preliminary Damage Assessment PDA process is a mechanism and the rest is boilerplate.")

  testthat::local_mocked_bindings(
    pdf_text = function(pdf, ...) report_text,
    .package = "pdftools")

  attributes_read <- extract_pda_attributes("PDAReport_FEMA-1736-DR-MO.pdf")

  expect_equal(attributes_read$pa_cost_estimate_total, 28931081)
  expect_equal(attributes_read$pa_primary_impact, "Cost for debris removal")
})
