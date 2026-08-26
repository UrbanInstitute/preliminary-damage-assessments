#' Return one row per record-county for every PDA record
#'
#' @description
#' `get_preliminary_damage_assessments()` returns one observation per request.
#' This function returns one observation per request-county, drawing the county
#' detail from two sources: the comma-separated county listing that many PDA
#' reports print, and FEMA's own record of the areas a declaration designated.
#' Every record passed in is represented in the output. A record with neither
#' source of county detail--a denial with no matched report, a tribal
#' request--keeps a single row with the county columns empty.
#'
#' @details
#' # Where the county rows come from
#'
#' Two columns record which source named a county, and a county named by both
#' is one row with both columns `TRUE` rather than two rows:
#'
#' - `pda_county_indicator` is `TRUE` when the county appeared in the report's own county
#'   listing. Only reports that requested Public Assistance and printed a
#'   countywide breakdown carry a listing--roughly two thirds of all PDAs.
#' - `fema_county_indicator` is `TRUE` when the county appeared in FEMA's designated areas
#'   for the declaration, including when it arrived through a statewide
#'   designation. Only approvals have designated areas; a denial never receives
#'   a disaster number and so has none.
#'
#' These two are distinct from `pda_county_name`, `pda_county_geoid`, and
#' `pda_county_name_reported`, which say what the county is rather than whether
#' the PDA listing named it.
#'
#' Because a declaration designates about twenty counties on average, and a
#' statewide designation expands to every county in the state, this function
#' returns many times more rows than the request-level dataset it is given--tens
#' of thousands of county rows for the full archive.
#'
#' # Statewide designations and the vintage of the county list
#'
#' A designated area that FEMA names "Statewide" covers the whole state rather
#' than one county, and is expanded into one row per county in that state, each
#' with `fema_statewide_request` set to `TRUE`. The counties used for that
#' expansion come from a single recent vintage of `tidycensus::fips_codes`,
#' applied to every year of the archive. For statewide requests, therefore, the
#' county-level observations may differ slightly from the counties that actually
#' existed at the time of the declaration. Connecticut is the clearest case: a
#' statewide Connecticut declaration from 2011 comes back as the nine planning
#' regions that replaced the state's counties in 2022, not as the eight counties
#' that existed on the day of the declaration.
#'
#' # Places that are not counties
#'
#' The listing mixes counties with the other quasi-county geographies:
#' Louisiana parishes, Alaska boroughs and census areas, Virginia and Missouri
#' independent cities, Puerto Rico municipios. Tribal nations are included
#' when they are subrecipients on state-led requests, as are Alaskan Regional Education
#' Attendance Areas, though neither have county FIPS codes. `geography_type` provides these
#' details. FEMA's designated areas name tribal lands and other non-county
#' places the same way; those rows keep the name FEMA gave them in
#' `fema_designated_area` and carry no county code.
#'
#' # Values of zero
#'
#' A zero is not necessarily a county assessed at no damage. Often, this may reflect
#' when a county-level PDA is ongoing, or perhaps when one was not completed.
#' The value is preserved as printed and the row is flagged in the warnings column
#' so that zeros can be excluded as appropriate.
#'
#' # Identifying an event
#'
#' Every column of the request-level record is repeated down that record's
#' county rows, so `fema_declaration_request_number` -- FEMA's own identifier
#' for the declaration request, never missing for an approval or a denial --
#' still names the event each row belongs to. Group by
#' `fema_declaration_request_number` to get back to one row per event.
#'
#' That column and `pda_county_geoid` together are the uniqueness key of the
#' returned data, with two exceptions: the repeated county listing described
#' below, and rows that have no county code at all, where several designated
#' areas under one event -- two reservations, say -- are separate rows told
#' apart by `fema_designated_area`.
#'
#' # Counties listed twice
#'
#' A few reports cover two separate incidents and print the whole county list
#' once for each, giving a different per capita figure each time. Both figures
#' are returned as separate rows, since neither supersedes the other, and both
#' rows are flagged in the warnings column.
#'
#' @param pda_df A dataframe returned by [get_preliminary_damage_assessments()].
#' @param declaration_areas A dataframe of FEMA's designated areas, one row per
#'   declaration-area, with the columns `fetch_declaration_areas()` returns. The
#'   default fetches them from FEMA, so this function needs a network connection
#'   whenever `pda_df` carries a disaster number column; pass a dataframe to
#'   supply them from elsewhere.
#'
#' @return `pda_df` with one row per record-county, and these columns added:
#'   \describe{
#'     \item{pda_county_name}{The county's name as the Census Bureau spells it, or,
#'        where the name could not be matched, the name as the report printed it.
#'        `NA` on a row with no county at all.}
#'     \item{pda_county_name_reported}{The name exactly as the report printed it,
#'        kept so that a failed match can be checked against the source. `NA` on
#'        a row that did not come from a county listing.}
#'     \item{pda_county_geoid}{Five-digit county FIPS code, `NA` where the place has
#'        no county code or its name could not be matched.}
#'     \item{pda_geography_type}{What kind of place the row describes: "county",
#'        "parish", "borough", "census area", "municipality", "municipio",
#'        "island", "independent city", "education area", "tribal entity", or
#'        "unrecognized" where the name matched nothing and resembles none of
#'        these. `NA` on a row with no county at all.}
#'     \item{pda_pa_per_capita_impact_county}{The county's per capita impact in
#'        dollars, as printed. `NA` on a row that did not come from a county
#'        listing.}
#'     \item{pda_county_indicator}{`TRUE` where the report's own county listing named this
#'        county.}
#'     \item{fema_county_indicator}{`TRUE` where FEMA's designated areas for the
#'        declaration named this county.}
#'     \item{fema_statewide_request}{`TRUE` where the row came from expanding a
#'        statewide designation rather than from a county FEMA named
#'        individually.}
#'     \item{fema_designated_area}{The name FEMA gave the designated area, kept
#'        so that tribal lands and other non-county designations stay
#'        identifiable. Where more than one designated area falls inside one
#'        county, the names are semicolon-separated.}
#'     \item{fema_ihp_declared_county, fema_ia_declared_county,
#'        fema_pa_declared_county, fema_hm_declared_county}{FEMA's record of the
#'        programs turned on for this designated area.}
#'   }
#'   Where a request-level column has a county-level counterpart, only the
#'   county-level one is returned: `pda_pa_per_capita_impact_countywide`, the
#'   raw text of the county listing, is dropped in favor of
#'   `pda_pa_per_capita_impact_county`, and the declaration-level
#'   `fema_ihp_declared`, `fema_ia_declared`, `fema_pa_declared`, and
#'   `fema_hm_declared` are dropped in favor of their `_county` versions.
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
transform_pda_counties = function(
    pda_df, declaration_areas = fetch_declaration_areas()) {

  ## Both paths of `get_preliminary_damage_assessments()` prefix every
  ## report-side column with `pda_`. They differ only in where the state name
  ## and the disaster number come from: the joined output carries FEMA's own
  ## `fema_state_name` and `fema_disaster_number`, the report-only output the
  ## text-derived `pda_state_name` and `pda_disaster_number`.
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

  ## With no disaster number there is nothing to look FEMA's designated areas up
  ## by, so `declaration_areas` is never evaluated and no fetch is made.
  disaster_columns = intersect(
    c("fema_disaster_number", named("disaster_number")), names(pda_df))
  disaster_column = if (length(disaster_columns) > 0) {
    disaster_columns[1] } else { NA_character_ }

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

  ## Every record is followed through the function by this identifier, rather
  ## than by its report path, because a record with no matched report has no
  ## path and still has to come back out with a row of its own.
  records = pda_df %>%
    dplyr::mutate(record_id = dplyr::row_number())

  reports = records %>%
    dplyr::filter(!is.na(.data[[countywide_column]]))

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
      "\\b(First|Second|Third|Fourth)\\s+Incident\\b\\s*[-‐-―−:]?\\s*",
      ignore_case = TRUE)) %>%
    stringr::str_squish()

  ## Every entry in the listing is a run of words followed by that place's
  ## figure, so each number marks the end of one entry and the words before it
  ## are the name. The name may hold accented letters and curly apostrophes,
  ## both of which appear in the source text: with a plain ASCII character
  ## class, "Prince George's County" is read as "s County".
  entry_pattern = stringr::str_c(
    "([[:alpha:]][[:alpha:]À-ɏ'‘’.\\- ]*?)",
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

  ## Every county the Census has a code for, including the codes it has since
  ## retired, because a declaration from 2011 designated the counties of 2011.
  ## The name match and the fill-a-name-from-a-code lookup both want the whole
  ## list; only the statewide expansion wants the current vintage alone.
  county_lookup = county_reference()

  counties = county_lookup %>%
    dplyr::transmute(
      county_geoid,
      census_county_name,
      state_key = normalize_geography_name(state_name),
      county_key = normalize_geography_name(census_county_name))

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
      geography_type = dplyr::coalesce(
        geography_type, classify_geography_type(county_name, county_geoid)))

  ## Each row's problems are collected as a list of descriptions and folded into
  ## the warnings column at the end, so that a row carrying two problems reports
  ## both, and so that the request-level warnings the record already held are
  ## kept rather than overwritten. Only rows read out of a county listing are
  ## examined: the rows built from FEMA's designated areas carry no per capita
  ## value and no reported name, and a county named by both sources must not be
  ## read as a county the report listed twice.
  flagged = matched1 %>%
    dplyr::mutate(
      is_repeated = dplyr::n() > 1,
      .by = dplyr::all_of(c("record_id", "county_name_reported"))) %>%
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

  ## FEMA's designated areas, reduced to one row per declaration-county. A
  ## record with no disaster number -- a denial, or a report whose number could
  ## not be read -- reaches none of them.
  areas = prepare_declaration_areas(
    if (is.na(disaster_column)) { NULL } else { declaration_areas },
    county_lookup)

  record_areas = if (is.na(disaster_column)) {
    areas[0, ] %>% dplyr::mutate(record_id = integer(0)) %>%
      dplyr::select(-"disaster_number")
  } else {
    records %>%
      dplyr::transmute(
        record_id, disaster_number = as.character(.data[[disaster_column]])) %>%
      dplyr::filter(!is.na(disaster_number)) %>%
      tidylog::inner_join(
        areas, by = "disaster_number", relationship = "many-to-many") %>%
      dplyr::select(-"disaster_number") }

  ## A county named by both sources is one row, so the designated areas are
  ## joined onto the listing rows rather than added beside them. Listing rows
  ## whose county code is missing are left unjoined: an unmatched name and an
  ## un-coded designated area are not the same place merely because neither has
  ## a code.
  area_rows_by_county = record_areas %>%
    dplyr::filter(!is.na(county_geoid))

  listing_rows = flagged %>%
    tidylog::left_join(
      area_rows_by_county, by = c("record_id", "county_geoid"),
      relationship = "many-to-one") %>%
    dplyr::mutate(
      pda_county_indicator = TRUE,
      fema_county_indicator = dplyr::coalesce(fema_county_indicator, FALSE),
      fema_statewide_request = dplyr::coalesce(fema_statewide_request, FALSE))

  ## The designated counties the listing did not name become extra rows for that
  ## record -- which, for a record with no listing at all, is every county FEMA
  ## designated.
  listing_keys = flagged %>%
    dplyr::filter(!is.na(county_geoid)) %>%
    dplyr::distinct(record_id, county_geoid)

  area_rows = records %>%
    tidylog::inner_join(
      record_areas %>%
        tidylog::anti_join(listing_keys, by = c("record_id", "county_geoid")),
      by = "record_id", relationship = "one-to-many") %>%
    dplyr::mutate(pda_county_indicator = FALSE)

  ## A record neither source describes -- a denial with no matched report, a
  ## tribal request -- keeps one row with the county columns empty, so that the
  ## output covers every record it was given.
  described_records = union(flagged$record_id, area_rows$record_id)

  placeholder_rows = records %>%
    dplyr::filter(!record_id %in% described_records) %>%
    dplyr::mutate(
      county_geoid = NA_character_,
      pda_county_indicator = FALSE,
      fema_county_indicator = FALSE,
      fema_statewide_request = FALSE)

  ## Neither kind of row came from a county listing, so the name is taken from
  ## the county code and the reported name and per capita value stay empty.
  non_listing_rows = dplyr::bind_rows(area_rows, placeholder_rows) %>%
    tidylog::left_join(
      county_lookup %>% dplyr::select("county_geoid", "census_county_name"),
      by = "county_geoid", relationship = "many-to-one") %>%
    dplyr::mutate(
      county_name = census_county_name,
      county_name_reported = NA_character_,
      pa_per_capita_impact_county = NA_real_,
      geography_type = classify_geography_type(county_name, county_geoid),
      county_notes = purrr::map(seq_len(dplyr::n()), ~ character(0)))

  combined = dplyr::bind_rows(listing_rows, non_listing_rows) %>%
    dplyr::arrange(record_id)

  result = combined %>%
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
      "county_notes", "record_id"))) %>%
    dplyr::rename_with(
      ~ named(.x),
      .cols = dplyr::all_of(c(
        "county_name", "county_name_reported", "county_geoid",
        "geography_type", "pa_per_capita_impact_county"))) %>%
    dplyr::relocate(
      dplyr::any_of(c(
        named(c("county_name", "county_name_reported", "county_geoid",
                "geography_type", "pa_per_capita_impact_county", "county_indicator")),
        "fema_county_indicator", "fema_statewide_request", "fema_designated_area",
        "fema_ihp_declared_county", "fema_ia_declared_county",
        "fema_pa_declared_county", "fema_hm_declared_county")),
      .after = dplyr::all_of(countywide_column)) %>%
    ## the county-level columns supersede their request-level counterparts, so
    ## the raw county listing text and the declaration-level program flags are
    ## dropped rather than repeated down every county row
    dplyr::select(-dplyr::any_of(c(
      countywide_column, "fema_ihp_declared", "fema_ia_declared",
      "fema_pa_declared", "fema_hm_declared")))

  ## An education area or a tribal entity has no county code to find, so its
  ## absence is not a failed match; every other listing row without one is. Rows
  ## built from FEMA's designated areas are excluded, since no name was read for
  ## them and none could have failed to match.
  is_listing_row = result[[named("county_indicator")]]
  n_unmatched = sum(
    is_listing_row &
      is.na(result[[named("county_geoid")]]) &
      !result[[named("geography_type")]] %in% c("education area", "tribal entity"))

  n_placeholder = nrow(placeholder_rows)
  n_areas_only = length(setdiff(area_rows$record_id, flagged$record_id))

  if (n_placeholder > 0 || n_areas_only > 0) {
    message(
      n_placeholder, " of ", nrow(pda_df), " record(s) have no county detail ",
      "from either source and are represented by a single row with the county ",
      "columns empty. ", n_areas_only, " record(s) have FEMA designated areas ",
      "but no PDA county listing.") }

  if (n_unmatched > 0) {
    warning(
      n_unmatched, " of ", sum(is_listing_row), " county listing row(s) carry ",
      "a name that matched no county in their state, usually a spelling error ",
      "in the source report.", call. = FALSE) }

  result
}

#' Fetch FEMA's record of the areas each major disaster declaration designated
#'
#' `DisasterDeclarationsSummaries` returns one row per designated area, almost
#' always a county, so a single declaration appears many times. `join_pda_outcomes()`
#' keeps one row per declaration and discards the areas; this returns them.
#'
#' @return A dataframe with one row per declaration-area.
#' @noRd
fetch_declaration_areas = function() {
  rfema::open_fema(
      data_set = "DisasterDeclarationsSummaries",
      ## major disaster declarations only; a PDA precedes a major disaster
      ## request, not an emergency declaration or a fire management grant
      filters = list(declarationType = "=DR"),
      ask_before_call = FALSE) %>%
    janitor::clean_names() %>%
    dplyr::select(dplyr::all_of(c(
      "disaster_number", "fips_state_code", "fips_county_code",
      "designated_area", "ih_program_declared", "ia_program_declared",
      "pa_program_declared", "hm_program_declared")))
}

#' Reduce FEMA's designated areas to one row per declaration-county
#'
#' A designated area FEMA names "Statewide" is expanded into one row per county
#' in that state. Other areas with a county code of "000" are tribal lands,
#' reservations, and similar places that sit inside no single county; they are
#' kept as rows of their own, named but without a county code, rather than
#' expanded. Where several designated areas fall inside one county -- a school
#' district and a regional educational attendance area, say -- they are folded
#' into that county's single row, with their names semicolon-separated and a
#' program counted as declared if any of them declared it.
#'
#' @param declaration_areas A dataframe from `fetch_declaration_areas()`, or
#'   `NULL` for no areas at all.
#' @param county_lookup The reference set from `county_reference()`.
#' @return A dataframe with one row per declaration-county.
#' @noRd
prepare_declaration_areas = function(declaration_areas, county_lookup) {

  empty = tibble::tibble(
    disaster_number = character(0),
    county_geoid = character(0),
    fema_designated_area = character(0),
    fema_ihp_declared_county = logical(0),
    fema_ia_declared_county = logical(0),
    fema_pa_declared_county = logical(0),
    fema_hm_declared_county = logical(0),
    fema_statewide_request = logical(0),
    fema_county_indicator = logical(0))

  if (is.null(declaration_areas) || nrow(declaration_areas) == 0) {
    return(empty) }

  designated = declaration_areas %>%
    dplyr::transmute(
      disaster_number = as.character(disaster_number),
      fips_state_code = as.character(fips_state_code),
      fips_county_code = as.character(fips_county_code),
      fema_designated_area = designated_area,
      fema_ihp_declared_county = as.logical(ih_program_declared),
      fema_ia_declared_county = as.logical(ia_program_declared),
      fema_pa_declared_county = as.logical(pa_program_declared),
      fema_hm_declared_county = as.logical(hm_program_declared),
      is_statewide = fips_county_code == "000" & designated_area == "Statewide")

  ## The vintage caveat in the function documentation lives here: a statewide
  ## designation from any year is expanded against today's counties.
  statewide = designated %>%
    dplyr::filter(is_statewide) %>%
    dplyr::select(-"fips_county_code") %>%
    tidylog::inner_join(
      county_lookup %>%
        dplyr::filter(is_current_county) %>%
        dplyr::transmute(fips_state_code = state_code, county_geoid),
      by = "fips_state_code", relationship = "many-to-many") %>%
    dplyr::mutate(fema_statewide_request = TRUE)

  by_county = designated %>%
    dplyr::filter(!is_statewide) %>%
    dplyr::mutate(
      county_geoid = dplyr::if_else(
        fips_county_code == "000", NA_character_,
        stringr::str_c(fips_state_code, fips_county_code)),
      fema_statewide_request = FALSE) %>%
    dplyr::select(-"fips_county_code")

  areas = dplyr::bind_rows(statewide, by_county) %>%
    dplyr::select(-dplyr::all_of(c("fips_state_code", "is_statewide")))

  ## Areas with no county code cannot be folded together, because two tribal
  ## designations under one declaration are two different places.
  folded = areas %>%
    dplyr::filter(!is.na(county_geoid)) %>%
    dplyr::summarise(
      fema_designated_area = stringr::str_c(
        unique(fema_designated_area), collapse = "; "),
      dplyr::across(
        dplyr::all_of(c(
          "fema_ihp_declared_county", "fema_ia_declared_county",
          "fema_pa_declared_county", "fema_hm_declared_county",
          "fema_statewide_request")),
        ~ any(.x, na.rm = TRUE)),
      .by = c("disaster_number", "county_geoid"))

  dplyr::bind_rows(folded, areas %>% dplyr::filter(is.na(county_geoid))) %>%
    dplyr::mutate(fema_county_indicator = TRUE)
}

#' Every county the Census has a FIPS code for, flagged by whether it still exists
#'
#' `tidycensus::fips_codes` holds 3,256 rows, twenty-one more than the 3,235
#' county equivalents that exist today. Twenty of the surplus are codes the
#' Census has retired but kept in the table, and the twenty-first is Midway
#' Islands, which is not a county equivalent at all. The retired codes are
#' listed here so that a statewide designation expands to the counties of one
#' vintage rather than to a mixture of vintages -- expanding Connecticut
#' straight from the table would return seventeen rows, its eight former
#' counties and the nine planning regions that replaced them in 2022.
#'
#' The retired codes are kept in the returned table rather than dropped, because
#' the name match and the fill-a-name-from-a-code lookup both need them: a
#' declaration from 2011 designated Hartford County, and a report from 2011
#' named it.
#'
#' Per-state counts were checked against the published number of county
#' equivalents for each state, and every discrepancy is accounted for below.
#'
#' @return A dataframe with one row per FIPS code: `county_geoid`, `state_code`,
#'   `state_name`, `census_county_name`, and `is_current_county`.
#' @noRd
county_reference = function() {

  discontinued_county_geoids = c(
    ## Connecticut replaced its eight counties with nine planning regions in
    ## 2022; both sets are in the table
    "09001", "09003", "09005", "09007", "09009", "09011", "09013", "09015",
    ## Alaska census areas that were split or renamed: Prince of Wales-Outer
    ## Ketchikan, two earlier forms of the Skagway-Hoonah-Angoon area,
    ## Valdez-Cordova, Wade Hampton (now Kusilvak), Wrangell-Petersburg
    "02201", "02231", "02232", "02261", "02270", "02280",
    ## Virginia independent cities that reverted to town status within a county
    "51515", "51560", "51780",
    ## Shannon County, South Dakota, renamed Oglala Lakota County in 2015
    "46113",
    ## Dade County, Florida, renamed Miami-Dade County in 1997
    "12025",
    ## Yellowstone National Park, Montana, which stopped being a county
    ## equivalent in 1997
    "30113")

  reference = tidycensus::fips_codes %>%
    dplyr::transmute(
      county_geoid = stringr::str_c(state_code, county_code),
      state_code,
      state_name,
      census_county_name = county) %>%
    dplyr::mutate(
      is_current_county = !county_geoid %in% discontinued_county_geoids)

  ## If the Census drops a retired code from the table, the list above has gone
  ## stale and the per-state counts should be checked again.
  unrecognized_codes = setdiff(discontinued_county_geoids, reference$county_geoid)
  if (length(unrecognized_codes) > 0) {
    warning(
      "tidycensus::fips_codes no longer holds ",
      stringr::str_c(unrecognized_codes, collapse = ", "),
      ", so the list of retired county codes needs revisiting.", call. = FALSE) }

  reference
}

#' Read what kind of place a name describes
#'
#' The type is read from the name, since the Census spells a place's type
#' consistently. A name that carries no type but does have a county code is a
#' place that is its own county equivalent and is named for nothing else -- the
#' District of Columbia, Guam. A row with no name at all describes no place, so
#' it gets no type.
#'
#' @param place_names A character vector of county or place names.
#' @param county_geoids A character vector of county FIPS codes, one per name.
#' @return A character vector of geography types, one per input.
#' @noRd
classify_geography_type = function(place_names, county_geoids) {
  dplyr::case_when(
    stringr::str_detect(place_names, "\\bCounty$") ~ "county",
    stringr::str_detect(place_names, "\\bParish$") ~ "parish",
    stringr::str_detect(place_names, "\\b(City and )?Borough$") ~ "borough",
    stringr::str_detect(place_names, "\\bCensus Area$") ~ "census area",
    stringr::str_detect(place_names, "\\bMunicipality$") ~ "municipality",
    stringr::str_detect(place_names, "\\bMunicipio$") ~ "municipio",
    stringr::str_detect(place_names, stringr::regex("\\bcity$", ignore_case = TRUE)) ~
      "independent city",
    stringr::str_detect(place_names, "\\bIsland$") ~ "island",
    is.na(place_names) ~ NA_character_,
    !is.na(county_geoids) ~ "county",
    TRUE ~ "unrecognized")
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
    "á" = "a", "í" = "i", "ó" = "o", "ñ" = "n",
    "ü" = "u")

  keys = place_names %>%
    stringr::str_replace_all("[‘’]", "'") %>%
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
  "census_county_name", "census_county_name_bare", "designated_area",
  "disaster_number", "fema_county_indicator", "fema_designated_area",
  "fema_statewide_request", "fips_county_code", "fips_state_code",
  "geography_type", "hm_program_declared", "ia_program_declared",
  "ih_program_declared", "is_current_county", "is_repeated", "is_statewide",
  "n_sharing_bare_name", "pa_per_capita_impact_county", "pa_program_declared",
  "record_id", "state_key", "state_code", "state_name"))
