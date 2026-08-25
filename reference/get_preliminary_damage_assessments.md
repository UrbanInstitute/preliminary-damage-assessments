# Get FEMA Preliminary Damage Assessments Report Data

Returns structured data extracted from PDF preliminary damage assessment
(PDA) reports.

## Usage

``` r
get_preliminary_damage_assessments(
  file_path,
  directory_path = NULL,
  use_cache = TRUE,
  join_outcomes = TRUE
)
```

## Arguments

- file_path:

  The file path to the cached dataset, or if there is no cache, the path
  at which to cache the resulting data.

- directory_path:

  The path to the directory where PDA PDFs are stored. These files are
  not fetched by this function; run
  [`scrape_pda_pdfs()`](https://UrbanInstitute.github.io/preliminarydamages/reference/scrape_pda_pdfs.md)
  to download them and to refresh the archive as FEMA publishes new
  reports.

- use_cache:

  Boolean, default is TRUE. Read the existing dataset stored at
  `file_path`? If FALSE, data will be generated anew. Else, if a file
  exists at `file_path`, this file will be returned.

- join_outcomes:

  Boolean, default is TRUE. Return FEMA's authoritative record of every
  granted and denied major disaster request, with the PDA attributes
  joined onto it? If FALSE, one row per PDA report is returned instead,
  without the declaration and denial columns.

## Value

A dataframe with one row per FEMA declaration or denial record (or, when
`join_outcomes = FALSE`, one row per PDA report). Several fields have no
labelled equivalent in the source documents and are derived from the
report text; the logic used is given with each.

Every column name carries a prefix saying where its value came from:
`fema_` for FEMA's own declaration and denial records, `pda_` for values
read out of the PDF reports. A column keeps the same name under either
setting. When `join_outcomes = FALSE`, the columns describing the FEMA
record or the match to it – `fema_disaster_number`, `fema_state_name`,
`fema_state_fips`, `fema_decision`, `fema_decision_date`,
`fema_decision_year`, `fema_declaration_request_date`,
`fema_declaration_title`, `fema_requested_incident_types`,
`fema_hazards`, the eight `fema_*_declared`/`fema_*_requested` program
fields, `fema_tribal_request`, `pda_matched`, and `pda_match_quality` –
are absent, and three columns present only on that path take their
place: `pda_disaster_number`, the disaster number the report itself
prints, and `pda_state_name` and `pda_state_fips`, the state read out of
the report text (described under `fema_state_name` and `fema_state_fips`
below, which hold FEMA's own value for a joined record). Columns
include:

- fema_disaster_number:

  FEMA disaster number. Denied requests are always NA.

- fema_state_name:

  The requesting state or territory. A tribal request carries the state
  the tribe's lands lie in, as recorded by FEMA.

- fema_state_fips:

  The two-digit FIPS code corresponding to `fema_state_name`.

- fema_decision:

  "Approved" or "Denied".

- fema_decision_date:

  The date FEMA settled the request.

- fema_decision_year:

  The calendar year of `fema_decision_date`.

- fema_declaration_request_date:

  Date the state filed the request (denied records only).

- fema_declaration_title:

  FEMA's name for the event.

- fema_requested_incident_types:

  FEMA's classification of the requested incident types (denied records
  only).

- fema_hazards:

  Semicolon-separated hazard categories read from FEMA's
  `fema_declaration_title` and `fema_requested_incident_types`.

- fema_ihp_declared, fema_ia_declared, fema_pa_declared,
  fema_hm_declared:

  FEMA's record of whether the declaration authorized the Individuals
  and Households Program, the (pre-2002) Individual Assistance program,
  Public Assistance, and Hazard Mitigation. NA for denied requests. A
  program can be declared without appearing in the PDA – added by a
  later request – so these legitimately diverge from the
  `pda_*_requested` flags.

- fema_ihp_requested, fema_ia_requested, fema_pa_requested,
  fema_hm_requested:

  FEMA's record of the programs the denied request asked for. NA for
  approvals. Requests for the Individual Assistance umbrella programs
  are recorded under `fema_ihp_requested`; `fema_ia_requested` names the
  pre-2002 Individual Assistance program and is FALSE throughout the
  years the PDA archive covers. `fema_ihp_requested` is narrower than
  `pda_ia_requested`: a request only for a component program such as
  Disaster Unemployment Assistance sets the `pda_` flag but not FEMA's.

- fema_tribal_request:

  FEMA's record of whether a tribal government made the request.

- pda_matched:

  TRUE where a PDA report was matched to the FEMA observation.

- pda_warnings:

  Semicolon-separated descriptions of potential data quality problems.

- pda_match_quality:

  How the match was made. "exact" covers an approved request joined on
  its disaster number; a denied request whose determination date is the
  denial decision date within the same state; and a denied request whose
  report names no state – a tribal report names the tribe instead –
  matched on the determination date alone, where exactly one denial in
  the country shares that date and it agrees with the report on hazard
  or shares a word with its title. "approximate" is a denied request
  matched instead to the nearest denial of the same hazard type within
  seven days either side of the date the report prints, and says how
  many days apart the two records are. "manual" is a link established by
  hand.

- pda_path:

  The local file path to the source PDA PDF.

- pda_decision:

  One of "approved", "denial", "appeal_approved", or "appeal_denial".

- pda_declaration_title:

  The report's description of the event.

- pda_date_determined:

  Date the PDA determination was made.

- pda_tribal_flag:

  1 where the request came from a tribal government, 0 otherwise.

- pda_tribal_name:

  The tribe that made the request.

- pda_tribal_fips:

  The `GEOID` of the matching area in
  [`tigris::native_areas()`](https://rdrr.io/pkg/tigris/man/native_areas.html)
  (2023 vintage). NA where no area could be matched to the tribe's name.

- pda_hazards:

  Semicolon-separated hazard categories read from
  `pda_declaration_title`.

- pda_pa_requested:

  TRUE where Public Assistance was requested, FALSE where the report
  states "Public Assistance - Not requested".

- pda_pa_preemptive_declaration:

  1 where the report states that the "requirement for a joint PDA may be
  waived", meaning the event was severe enough that FEMA proceeded
  without conducting a joint preliminary damage assessment first; 0
  otherwise.

- pda_pa_primary_impact:

  The primary type of impact listed for PA.

- pda_pa_cost_estimate_total:

  Estimated total PA cost, nominal dollars.

- pda_pa_per_capita_impact_statewide:

  Statewide (or territory/commonwealth-wide) per capita impact amount.

- pda_pa_per_capita_impact_indicator_statewide:

  FEMA's statutory statewide per capita impact threshold, nominal
  dollars.

- pda_pa_per_capita_impact_countywide:

  Raw text of countywide per capita impact ratios. Pass the data to
  [`transform_pda_counties()`](https://UrbanInstitute.github.io/preliminarydamages/reference/transform_pda_counties.md)
  to split this text into one row per county, each with its own county
  FIPS code and per capita impact.

- pda_pa_per_capita_impact_indicator_countywide:

  FEMA's statutory countywide per capita threshold in dollars.

- pda_pa_per_capita_impact_countywide_max:

  Maximum countywide per capita impact ratio parsed from
  `pa_per_capita_impact_countywide`.

- pda_pa_per_capita_impact_countywide_min:

  Minimum countywide per capita impact ratio parsed from
  `pa_per_capita_impact_countywide`.

- pda_pa_threshold_ratio:

  `pa_per_capita_impact_statewide` divided by
  `pa_per_capita_impact_indicator_statewide`: the estimated per capita
  damage expressed as a multiple of the statutory threshold; a value
  above 1 indicates damages exceeded the threshold.

- pda_ia_requested:

  TRUE where Individual Assistance was requested, FALSE otherwise.
  Individual Assistance is read broadly as any of Individuals and
  Households Program, Crisis Counseling, Disaster Unemployment
  Assistance, Disaster Legal Services, and Disaster Case Management, so
  this flag can be TRUE where FEMA's own `fema_ihp_requested` is FALSE.

- pda_requested_from_narrative:

  Names the program whose requested flag was settled from the report's
  opening narrative ("ia", "pa", or "ia; pa"), NA otherwise. The
  narrative is consulted only where the report disagrees with itself:
  the report summary says a program was not requested while the report
  prints values for it, or the report prints no values while the
  narrative does not name the program.

- pda_ia_residences_impacted:

  Total residences impacted.

- pda_ia_residences_destroyed:

  Number of residences destroyed.

- pda_ia_residences_major_damage:

  Number of residences with major damage.

- pda_ia_residences_minor_damage:

  Number of residences with minor damage.

- pda_ia_residences_affected:

  Number of residences affected (lowest damage category).

- pda_ia_residences_insured_total_percent:

  Percentage of impacted residences with any insurance coverage.

- pda_ia_residences_insured_flood_percent:

  Percentage of impacted residences with flood insurance coverage.

- pda_ia_households_poverty_percent:

  Percentage of households in poverty (or low income, depending on
  report vintage).

- pda_ia_households_owner_percent:

  Percentage of households that are owner-occupied.

- pda_ia_population_other_government_assistance_percent:

  Percentage of the population receiving other government assistance
  (e.g. SSI, SNAP).

- pda_ia_pre_disaster_unemployment_percent:

  Pre-disaster unemployment rate.

- pda_ia_65plus_percent:

  Percentage of the population age 65 and older.

- pda_ia_18below_percent:

  Percentage of the population age 18 and under.

- pda_ia_disability_percent:

  Percentage of the population with a disability.

- pda_ia_ihp_cost_to_capacity_ratio:

  Individuals and Households Program (IHP) Cost to Capacity (ICC) ratio.

- pda_ia_cost_estimate_total:

  Estimated total Individual Assistance cost.

- pda_cost_estimate_ia_pa_total:

  `pa_cost_estimate_total` plus `ia_cost_estimate_total`; NA only where
  both are missing.

- pda_text:

  The cleaned text extracted from the PDA PDF used to derive the fields
  above.

## Details

Data are extracted from PDF reports hosted at
<https://www.fema.gov/disaster/how-declared/preliminary-damage-assessments/reports>.
Owing to the unstructured nature of the source documents, some fields
may be incorrect in the data returned by the function, though
significant quality checks have been implemented in an effort to produce
a high-quality dataset.

With `join_outcomes = TRUE` (the default), the PDA data are attached to
FEMA's own structured records of which declaration requests were
declared and denied. FEMA's `DisasterDeclarationsSummaries` (granted
major disaster declarations) and `DeclarationDenials` (turned-down major
disaster requests) datasets are authoritative, so every such record is
returned, with the PDA columns empty where no report could be matched to
it. An approved request is matched on its disaster number. A denied
request never receives a disaster number, so it is matched on state, on
the decision date the report prints, and on agreement between the
hazards the two records describe, with anything ambiguous left
unmatched; `match_quality` records which of these applied. Set
`join_outcomes = FALSE` for the reports alone.

Data quality is reported through a single consolidated warning: how many
records carry values that may be incorrect – each such record's specific
issue(s) are written to the `pda_warnings` column – and how many PDA
reports could not be matched to an authoritative FEMA declaration or
denial and are therefore absent from the returned data.

## Examples

``` r
if (FALSE) { # \dontrun{
get_preliminary_damage_assessments(
  file_path = file.path("data", "pdas.csv"),
  directory_path = file.path("data", "pdfs"))
} # }
```
