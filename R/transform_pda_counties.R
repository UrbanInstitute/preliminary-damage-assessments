#' Split the county listing in a PDA report into one row per county-request
#'
#' @description
#' `get_preliminary_damage_assessments()` returns one observation per request,
#' but county-level per-capita impacts are specified in many PDAs as a single, 
#' comma-separated string. This function splits that text into one row per county-request,
#' including a county FIPS code and the county's estimated per-capita impact.
#'
#'
#' @details
#' # What the county listing covers
#'
#' Only reports that requested Public Assistance and printed a countywide
#' breakdown carry a listing--roughly two thirds of all PDAs. Tribal-led requests
#' never delineate counties, and they are not returned by this function, akin to
#' other reports with no county-level listings. 
#'
#' # Places that are not counties
#'
#' The listing mixes counties with the other quasi-county geographies:
#' Louisiana parishes, Alaska boroughs and census areas, Virginia and Missouri
#' independent cities, Puerto Rico municipios. Tribal nations are included
#' when they are subrecipients on state-led requests, as are Alaskan Regional Education
#' Attendance Areas, though neither have county FIPS codes. `geography_type` provides these
#' details.
#'
#' # Values of zero
#'
#' A zero is not necessarily a county assessed at no damage. Often, this may reflect
#' when a county-level PDA is ongoing, or perhaps when one was not completed. 
#' The value is preserved as printed and the row is flagged in the warnings column 
#' so that zeros can be excluded as appropriate.
#'
#' # Counties listed twice
#'
#' A few reports cover two separate incidents and print the whole county list
#' once for each, giving a different per capita figure each time. Both figures
#' are returned as separate rows, since neither supersedes the other, and both
#' rows are flagged in the warnings column. The report and county together are
#' therefore not inherently a unique key (though in most cases they are).
#'
#' @param pda_df A dataframe returned by [get_preliminary_damage_assessments()].
#'
#' @return `pda_df` with one row per report-county, and these columns added:
#'   \describe{
#'     \item{pda_county_name}{The county's name as the Census Bureau spells it, or,
#'        where the name could not be matched, the name as the report printed it.}
#'     \item{pda_county_name_reported}{The name exactly as the report printed it,
#'        kept so that a failed match can be checked against the source.}
#'     \item{pda_county_geoid}{Five-digit county FIPS code, `NA` where the place has
#'        no county code or its name could not be matched.}
#'     \item{pda_geography_type}{What kind of place the row describes: "county",
#'        "parish", "borough", "census area", "municipality", "municipio",
#'        "island", "independent city", "education area", "tribal entity", or
#'        "unrecognized" where the name matched nothing and resembles none of
#'        these.}
#'     \item{pda_pa_per_capita_impact_county}{The county's per capita impact in
#'        dollars, as printed.}
#'   }
#'   Problems found with a row are appended to the existing `pda_warnings`
#'   column, semicolon-separated, alongside any the request-level record
#'   already carried.
#' @export
#'
#' @examples
#' \dontrun{
#' get_preliminary_damage_assessments() %>%
#'   transform_pda_counties()
#' }
transform_pda_counties = function(pda_df) {

  ## Both paths of `get_preliminary_damage_assessments()` prefix every
  ## report-side column with `pda_`. They differ only in where the state name
  ## comes from: the joined output carries FEMA's own `fema_state_name`, the
  ## report-only output the text-derived `pda_state_name`.
  named = function(column) { stringr::str_c("pda_", column) }

  countywide_column = named("pa_per_capita_impact_countywide")
  state_column = if ("fema_state_name" %in% names(pda_df)) {
    "fema_state_name" } else { named("state_name") }
  warnings_column = named("warnings")
  path_column = named("path")

  required_columns = c(
    countywide_column, state_column, path_column, warnings_column)
  missing_columns = setdiff(required_columns, names(pda_df))
  if (length(missing_columns) > 0) {
    stop(
      "`pda_df` is missing ", stringr::str_c(missing_columns, collapse = ", "),
      ". Pass the dataframe returned by get_preliminary_damage_assessments().",
      call. = FALSE) }

  ## A value this many dollars per person is treated as a mis-extraction rather
  ## than a real figure. The bound is deliberately far above the largest genuine
  ## value in the archive (about $2,600 per person, in a county of a few
  ## hundred people) so that only clear failures are flagged.
  implausible_value = 10000

  ## TO DO: the handful of rows this catches are not a fault of the split. In a
  ## few reports the extraction of `pa_per_capita_impact_countywide` itself runs
  ## past the end of its section and sweeps in text from the next one, so the
  ## field holds a sentence about primary impact or total cost rather than a
  ## county listing. The fix belongs in `extract_pda_attributes()` in
  ## get_preliminary_damage_assessments.R, which is left alone here; until then
  ## these rows are flagged rather than dropped.
  section_bleed_pattern = stringr::regex(
    "per capita impact|primary impact|cost estimate", ignore_case = TRUE)

  reports = pda_df %>%
    dplyr::filter(!is.na(.data[[countywide_column]]))
  n_dropped = nrow(pda_df) - nrow(reports)

  ## Some reports close the listing with a sentence explaining it -- which
  ## counties were not assessed, or which reservation's costs are counted inside
  ## which county. Those sentences hold no county-value pairs and are cut before
  ## the split, so their capitalized words are not read as county names.
  listing_text = reports[[countywide_column]] %>%
    stringr::str_remove("\\bJoint PDAs?\\b.*$") %>%
    stringr::str_remove("\\bThe costs? for\\b.*$") %>%
    stringr::str_remove("\\bThe [A-Z][a-z]+ (Nation|Tribe|Band)\\b.*$") %>%
    stringr::str_remove("\\b[Tt]he [Pp]reliminary [Dd]amage.*$") %>%
    ## A report covering two incidents heads each county list with a label.
    ## The label is not part of the first county's name, and without this it
    ## would be read as one ("First Incident - Adams County").
    stringr::str_remove_all(stringr::regex(
      "\\b(First|Second|Third|Fourth)\\s+Incident\\b\\s*[-\u2010-\u2015\u2212:]?\\s*",
      ignore_case = TRUE)) %>%
    stringr::str_squish()

  ## Every entry in the listing is a run of words followed by that place's
  ## figure, so each number marks the end of one entry and the words before it
  ## are the name. The name may hold accented letters and curly apostrophes,
  ## both of which appear in the source text: with a plain ASCII character
  ## class, "Prince George's County" is read as "s County".
  entry_pattern = stringr::str_c(
    "([[:alpha:]][[:alpha:]\u00c0-\u024f'\u2018\u2019.\\- ]*?)",
    "\\s+([0-9]{1,5}(?:\\.[0-9]{1,3})?)")
  entries = stringr::str_match_all(listing_text, entry_pattern)

  long = reports %>%
    dplyr::mutate(
      county_name_reported = purrr::map(entries, ~ .x[, 2]),
      pa_per_capita_impact_county = purrr::map(entries, ~ as.numeric(.x[, 3]))) %>%
    tidyr::unnest(c(county_name_reported, pa_per_capita_impact_county)) %>%
    dplyr::mutate(
      ## the "and" that introduces the last entry of a list, and the "the" some
      ## reports put before a tribal entity, belong to the sentence rather than
      ## to the name
      county_name_reported = county_name_reported %>%
        stringr::str_remove(stringr::regex("^((and|the)\\s+)+", ignore_case = TRUE)) %>%
        stringr::str_squish())

  ## A run of words with a number after it is not always an entry: a report that
  ## writes a date or a stray conjunction inside the listing produces a fragment
  ## that names no place. Only fragments that cannot be a place name at all are
  ## discarded here -- an empty name, or a bare month -- so that a misspelled
  ## county name is still returned and flagged rather than silently lost.
  months = c(
    "January", "February", "March", "April", "May", "June", "July", "August",
    "September", "October", "November", "December") %>%
    stringr::str_c(collapse = "|")
  is_prose_fragment = stringr::str_detect(
    long$county_name_reported,
    stringr::regex(stringr::str_c("^$|^(and|the|", months, ")$"), ignore_case = TRUE))
  n_prose_fragments = sum(is_prose_fragment)
  long = long %>% dplyr::filter(!is_prose_fragment)

  ## Places with no county code are settled before the match is attempted, so
  ## that a school district or a tribe is never forced onto the county whose
  ## name it happens to share.
  long1 = long %>%
    dplyr::mutate(
      geography_type = dplyr::case_when(
        stringr::str_detect(
          county_name_reported,
          stringr::regex("REAA|Regional Educational? Attendance Area", ignore_case = TRUE)) ~
          "education area",
        stringr::str_detect(
          county_name_reported,
          "\\b(Tribes?|Nation|Community|Band|Pueblo|Reservation|Indians)\\b") ~
          "tribal entity",
        TRUE ~ NA_character_),
      county_key = dplyr::if_else(
        is.na(geography_type), county_name_reported, NA_character_) %>%
        normalize_geography_name(),
      state_key = normalize_geography_name(.data[[state_column]]))

  counties = tidycensus::fips_codes %>%
    dplyr::transmute(
      county_geoid = stringr::str_c(state_code, county_code),
      census_county_name = county,
      state_key = normalize_geography_name(state_name),
      county_key = normalize_geography_name(county))

  ## FEMA writes some names without their type ("Blount" for "Blount County")
  ## and gives others a type the Census spells differently ("Orocovis
  ## Municipality" for "Orocovis Municipio"), so a name that does not match
  ## whole is tried again with the type dropped from both sides.
  type_suffix = stringr::str_c(
    "\\s+(county|parish|borough|census area|city and borough|municipality|",
    "municipio|island|city)$")

  bare_counties = counties %>%
    dplyr::mutate(county_key = stringr::str_remove(county_key, type_suffix)) %>%
    ## an Alaska borough and a census area can reduce to the same bare name, and
    ## either answer would be a guess, so neither is offered
    dplyr::add_count(state_key, county_key, name = "n_sharing_bare_name") %>%
    dplyr::filter(n_sharing_bare_name == 1) %>%
    dplyr::select(-n_sharing_bare_name)

  matched = long1 %>%
    tidylog::left_join(
      counties, by = c("state_key", "county_key"), relationship = "many-to-one") %>%
    dplyr::mutate(county_key_bare = dplyr::if_else(
      is.na(county_geoid), stringr::str_remove(county_key, type_suffix), NA_character_)) %>%
    tidylog::left_join(
      bare_counties %>%
        dplyr::rename(
          county_key_bare = county_key,
          county_geoid_bare = county_geoid,
          census_county_name_bare = census_county_name),
      by = c("state_key", "county_key_bare"), relationship = "many-to-one") %>%
    dplyr::mutate(
      county_geoid = dplyr::coalesce(county_geoid, county_geoid_bare),
      census_county_name = dplyr::coalesce(census_county_name, census_county_name_bare))

  ## The kind of place a row describes is read from the type the Census gives
  ## the matched name, since that spelling is consistent; only where nothing
  ## matched does it fall back to what the report printed.
  matched1 = matched %>%
    dplyr::mutate(
      county_name = dplyr::coalesce(census_county_name, county_name_reported),
      geography_type = dplyr::case_when(
        !is.na(geography_type) ~ geography_type,
        stringr::str_detect(county_name, "\\bCounty$") ~ "county",
        stringr::str_detect(county_name, "\\bParish$") ~ "parish",
        stringr::str_detect(county_name, "\\b(City and )?Borough$") ~ "borough",
        stringr::str_detect(county_name, "\\bCensus Area$") ~ "census area",
        stringr::str_detect(county_name, "\\bMunicipality$") ~ "municipality",
        stringr::str_detect(county_name, "\\bMunicipio$") ~ "municipio",
        stringr::str_detect(county_name, stringr::regex("\\bcity$", ignore_case = TRUE)) ~
          "independent city",
        stringr::str_detect(county_name, "\\bIsland$") ~ "island",
        ## a name that matched a county code but carries no type is a place that
        ## is its own county equivalent and is named for nothing else -- the
        ## District of Columbia, Guam
        !is.na(county_geoid) ~ "county",
        TRUE ~ "unrecognized"))

  ## Each row's problems are collected as a list of descriptions and folded into
  ## the warnings column at the end, so that a row carrying two problems reports
  ## both, and so that the request-level warnings the record already held are
  ## kept rather than overwritten.
  flagged = matched1 %>%
    dplyr::mutate(
      is_repeated = dplyr::n() > 1,
      .by = dplyr::all_of(c(path_column, "county_name_reported"))) %>%
    dplyr::mutate(
      county_notes = purrr::pmap(
        list(is_repeated, pa_per_capita_impact_county, county_name_reported),
        function(repeated, value, name) {
          notes = character(0)
          if (isTRUE(repeated)) {
            notes = c(notes, stringr::str_c(
              name, " is listed more than once in this report, because the ",
              "report covers two separate incidents and prints a per capita ",
              "figure for each; both figures are returned, as neither ",
              "supersedes the other")) }
          if (!is.na(value) && value == 0) {
            notes = c(notes, stringr::str_c(
              name, " has a countywide per capita impact of 0, which marks a ",
              "county named in the request whose joint assessment had not been ",
              "completed when the report was written, not a county assessed at ",
              "no damage; the value is preserved as printed")) }
          if (!is.na(value) && value > implausible_value) {
            notes = c(notes, stringr::str_c(
              "the value read for ", name, " (", value, ") is far above any ",
              "plausible per capita figure, which means the extraction of ",
              "pa_per_capita_impact_countywide ran past the end of its section ",
              "in this report and swept in text from the next one")) }
          if (stringr::str_detect(name, section_bleed_pattern)) {
            notes = c(notes, stringr::str_c(
              "the name read for this row (", name, ") is not a place name, ",
              "which means the extraction of pa_per_capita_impact_countywide ",
              "ran past the end of its section in this report")) }
          notes }))

  result = flagged %>%
    dplyr::mutate(
      !!warnings_column := purrr::map2_chr(
        .data[[warnings_column]],
        county_notes,
        ~ {
          all_notes = c(if (is.na(.x)) { character(0) } else { .x }, .y)
          if (length(all_notes) == 0) { NA_character_ } else {
            stringr::str_c(all_notes, collapse = "; ") } })) %>%
    dplyr::select(-dplyr::any_of(c(
      "county_key", "county_key_bare", "state_key", "county_geoid_bare",
      "census_county_name_bare", "census_county_name", "is_repeated",
      "county_notes"))) %>%
    dplyr::rename_with(
      ~ named(.x),
      .cols = dplyr::all_of(c(
        "county_name", "county_name_reported", "county_geoid",
        "geography_type", "pa_per_capita_impact_county"))) %>%
    dplyr::relocate(
      dplyr::all_of(named(c(
        "county_name", "county_name_reported", "county_geoid",
        "geography_type", "pa_per_capita_impact_county"))),
      .after = dplyr::all_of(countywide_column))

  ## An education area or a tribal entity has no county code to find, so its
  ## absence is not a failed match; every other row without one is.
  n_unmatched = sum(
    is.na(result[[named("county_geoid")]]) &
      !result[[named("geography_type")]] %in% c("education area", "tribal entity"))

  parts = character(0)
  if (n_dropped > 0) {
    parts = c(parts, stringr::str_c(
      n_dropped, " of ", nrow(pda_df), " record(s) include no countywide ",
      "breakdown and are not represented here. Tribal requests never include ",
      "county details, and denied requests only do so relatively rarely.")) }
  if (n_unmatched > 0) {
    parts = c(parts, stringr::str_c(
      n_unmatched, " of ", nrow(result), " county row(s) carry a name that ",
      "matched no county in their state, usually a spelling error in the ",
      "source report.")) }
  if (length(parts) > 0) {
    warning(stringr::str_c(parts, collapse = " "), call. = FALSE) }

  result
}

#' Reduce a place name to a form that matches however it is spelled
#'
#' The reports and the Census disagree on the details of a name in ways that
#' never distinguish two different places: accented letters, curly apostrophes,
#' periods in abbreviations, "Saint" spelled out where the Census abbreviates
#' it, and hyphens where the Census uses them and the reports do not. All of
#' those are removed so that the two spellings of one place meet.
#'
#' The reports also name some places the long way round -- "City and Borough of
#' Juneau" where the Census writes "Juneau City and Borough" -- so a name in
#' that form is turned around. The Census gives Honolulu a plain "County", so
#' the "city and county" it is locally called reduces to that.
#'
#' @param place_names A character vector of state or county names.
#' @return A character vector of comparison keys, one per input.
#' @noRd
normalize_geography_name = function(place_names) {
  place_types = c(
    "city and county" = "county",
    "city and borough" = "city and borough",
    "independent city" = "city",
    "municipality" = "municipality",
    "municipio" = "municipio",
    "borough" = "borough",
    "parish" = "parish",
    "county" = "county",
    "city" = "city")

  inversion_pattern = stringr::str_c(
    "^(", stringr::str_c(names(place_types), collapse = "|"), ") of (.+)$")

  ## The Census writes every county name in plain ASCII, so a name the reports
  ## spell with an accent only ever has to lose it. These six characters are the
  ## whole of the non-ASCII text in the archive, so they are listed rather than
  ## transliterated by a general rule. The fold comes after the lower-casing, so
  ## only the lower-case forms are needed.
  accented_letters = c(
    "\u00e1" = "a", "\u00ed" = "i", "\u00f3" = "o", "\u00f1" = "n",
    "\u00fc" = "u")

  keys = place_names %>%
    stringr::str_replace_all("[\u2018\u2019]", "'") %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all(accented_letters) %>%
    stringr::str_remove_all("[.']") %>%
    stringr::str_replace_all("[-]", " ") %>%
    stringr::str_replace_all("\\bsaint\\b", "st") %>%
    stringr::str_squish() %>%
    stringr::str_remove("^((and|the) )+")

  inverted = stringr::str_match(keys, inversion_pattern)
  dplyr::if_else(
    is.na(inverted[, 1]),
    keys,
    stringr::str_c(inverted[, 3], " ", place_types[inverted[, 2]]))
}

utils::globalVariables(c(
  "county", "county_code", "county_name", "county_name_reported",
  "county_key", "county_geoid", "county_geoid_bare", "county_notes",
  "census_county_name", "census_county_name_bare", "geography_type",
  "is_repeated", "n_sharing_bare_name", "pa_per_capita_impact_county",
  "state_key", "state_code", "state_name"))
