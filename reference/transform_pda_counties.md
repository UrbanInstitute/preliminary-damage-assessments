# Return one row per record-county for every PDA record

[`get_preliminary_damage_assessments()`](https://UrbanInstitute.github.io/preliminary-damage-assessments/reference/get_preliminary_damage_assessments.md)
returns one observation per request. This function returns one
observation per request-county, drawing the county detail from two
sources: the comma-separated county listing that many PDA reports print,
and FEMA's own record of the areas a declaration designated. Every
record passed in is represented in the output. A record with neither
source of county detail–a denial with no matched report, a tribal
request–keeps a single row with the county columns empty.

## Usage

``` r
transform_pda_counties(pda_df, declaration_areas = fetch_declaration_areas())
```

## Arguments

- pda_df:

  A dataframe returned by
  [`get_preliminary_damage_assessments()`](https://UrbanInstitute.github.io/preliminary-damage-assessments/reference/get_preliminary_damage_assessments.md).

- declaration_areas:

  A dataframe of FEMA's designated areas, one row per declaration-area,
  with the columns `fetch_declaration_areas()` returns. The default
  fetches them from FEMA, so this function needs a network connection
  whenever `pda_df` carries a disaster number column; pass a dataframe
  to supply them from elsewhere.

## Value

`pda_df` with one row per record-county, and these columns added:

- pda_county_name:

  The county's name as the Census Bureau spells it, or, where the name
  could not be matched, the name as the report printed it. `NA` on a row
  with no county at all.

- pda_county_name_reported:

  The name exactly as the report printed it, kept so that a failed match
  can be checked against the source. `NA` on a row that did not come
  from a county listing.

- pda_county_geoid:

  Five-digit county FIPS code, `NA` where the place has no county code
  or its name could not be matched.

- pda_geography_type:

  What kind of place the row describes: "county", "parish", "borough",
  "census area", "municipality", "municipio", "island", "independent
  city", "education area", "tribal entity", or "unrecognized" where the
  name matched nothing and resembles none of these. `NA` on a row with
  no county at all.

- pda_pa_per_capita_impact_county:

  The county's per capita impact in dollars, as printed. `NA` on a row
  that did not come from a county listing.

- pda_county_indicator:

  `TRUE` where the report's own county listing named this county.

- fema_county_indicator:

  `TRUE` where FEMA's designated areas for the declaration named this
  county.

- fema_statewide_request:

  `TRUE` where the row came from expanding a statewide designation
  rather than from a county FEMA named individually.

- fema_designated_area:

  The name FEMA gave the designated area, kept so that tribal lands and
  other non-county designations stay identifiable. Where more than one
  designated area falls inside one county, the names are
  semicolon-separated.

- fema_ihp_declared_county, fema_ia_declared_county,
  fema_pa_declared_county, fema_hm_declared_county:

  FEMA's record of the programs turned on for this designated area.

Where a request-level column has a county-level counterpart, only the
county-level one is returned: `pda_pa_per_capita_impact_countywide`, the
raw text of the county listing, is dropped in favor of
`pda_pa_per_capita_impact_county`, and the declaration-level
`fema_ihp_declared`, `fema_ia_declared`, `fema_pa_declared`, and
`fema_hm_declared` are dropped in favor of their `_county` versions.
Problems found with a row are appended to the existing `pda_warnings`
column, semicolon-separated, alongside any the request-level record
already carried.

## Where the county rows come from

Two columns record which source named a county, and a county named by
both is one row with both columns `TRUE` rather than two rows:

- `pda_county_indicator` is `TRUE` when the county appeared in the
  report's own county listing. Only reports that requested Public
  Assistance and printed a countywide breakdown carry a listing–roughly
  two thirds of all PDAs.

- `fema_county_indicator` is `TRUE` when the county appeared in FEMA's
  designated areas for the declaration, including when it arrived
  through a statewide designation. Only approvals have designated areas;
  a denial never receives a disaster number and so has none.

These two are distinct from `pda_county_name`, `pda_county_geoid`, and
`pda_county_name_reported`, which say what the county is rather than
whether the PDA listing named it.

Because a declaration designates about twenty counties on average, and a
statewide designation expands to every county in the state, this
function returns many times more rows than the request-level dataset it
is given–tens of thousands of county rows for the full archive.

## Statewide designations and the vintage of the county list

A designated area that FEMA names "Statewide" covers the whole state
rather than one county, and is expanded into one row per county in that
state, each with `fema_statewide_request` set to `TRUE`. The counties
used for that expansion come from a single recent vintage of
[`tidycensus::fips_codes`](https://walker-data.com/tidycensus/reference/fips_codes.html),
applied to every year of the archive. For statewide requests, therefore,
the county-level observations may differ slightly from the counties that
actually existed at the time of the declaration. Connecticut is the
clearest case: a statewide Connecticut declaration from 2011 comes back
as the nine planning regions that replaced the state's counties in 2022,
not as the eight counties that existed on the day of the declaration.

## Places that are not counties

The listing mixes counties with the other quasi-county geographies:
Louisiana parishes, Alaska boroughs and census areas, Virginia and
Missouri independent cities, Puerto Rico municipios. Tribal nations are
included when they are subrecipients on state-led requests, as are
Alaskan Regional Education Attendance Areas, though neither have county
FIPS codes. `geography_type` provides these details. FEMA's designated
areas name tribal lands and other non-county places the same way; those
rows keep the name FEMA gave them in `fema_designated_area` and carry no
county code.

## Values of zero

A zero is not necessarily a county assessed at no damage. Often, this
may reflect when a county-level PDA is ongoing, or perhaps when one was
not completed. The value is preserved as printed and the row is flagged
in the warnings column so that zeros can be excluded as appropriate.

## Identifying an event

Every column of the request-level record is repeated down that record's
county rows, so `fema_declaration_request_number` – FEMA's own
identifier for the declaration request, never missing for an approval or
a denial – still names the event each row belongs to. Group by
`fema_declaration_request_number` to get back to one row per event.

That column and `pda_county_geoid` together are the uniqueness key of
the returned data, with two exceptions: the repeated county listing
described below, and rows that have no county code at all, where several
designated areas under one event – two reservations, say – are separate
rows told apart by `fema_designated_area`.

## Counties listed twice

A few reports cover two separate incidents and print the whole county
list once for each, giving a different per capita figure each time. Both
figures are returned as separate rows, since neither supersedes the
other, and both rows are flagged in the warnings column.

## Examples

``` r
if (FALSE) { # \dontrun{
get_preliminary_damage_assessments() %>%
  transform_pda_counties()
} # }
```
