# Split the county listing in a PDA report into one row per county-request

[`get_preliminary_damage_assessments()`](https://UrbanInstitute.github.io/preliminary-damage-assessments/reference/get_preliminary_damage_assessments.md)
returns one observation per request, but county-level per-capita impacts
are specified in many PDAs as a single, comma-separated string. This
function splits that text into one row per county-request, including a
county FIPS code and the county's estimated per-capita impact.

## Usage

``` r
transform_pda_counties(pda_df)
```

## Arguments

- pda_df:

  A dataframe returned by
  [`get_preliminary_damage_assessments()`](https://UrbanInstitute.github.io/preliminary-damage-assessments/reference/get_preliminary_damage_assessments.md).

## Value

`pda_df` with one row per report-county, and these columns added:

- pda_county_name:

  The county's name as the Census Bureau spells it, or, where the name
  could not be matched, the name as the report printed it.

- pda_county_name_reported:

  The name exactly as the report printed it, kept so that a failed match
  can be checked against the source.

- pda_county_geoid:

  Five-digit county FIPS code, `NA` where the place has no county code
  or its name could not be matched.

- pda_geography_type:

  What kind of place the row describes: "county", "parish", "borough",
  "census area", "municipality", "municipio", "island", "independent
  city", "education area", "tribal entity", or "unrecognized" where the
  name matched nothing and resembles none of these.

- pda_pa_per_capita_impact_county:

  The county's per capita impact in dollars, as printed.

Problems found with a row are appended to the existing `pda_warnings`
column, semicolon-separated, alongside any the request-level record
already carried.

## What the county listing covers

Only reports that requested Public Assistance and printed a countywide
breakdown carry a listing–roughly two thirds of all PDAs. Tribal-led
requests never delineate counties, and they are not returned by this
function, akin to other reports with no county-level listings.

## Places that are not counties

The listing mixes counties with the other quasi-county geographies:
Louisiana parishes, Alaska boroughs and census areas, Virginia and
Missouri independent cities, Puerto Rico municipios. Tribal nations are
included when they are subrecipients on state-led requests, as are
Alaskan Regional Education Attendance Areas, though neither have county
FIPS codes. `geography_type` provides these details.

## Values of zero

A zero is not necessarily a county assessed at no damage. Often, this
may reflect when a county-level PDA is ongoing, or perhaps when one was
not completed. The value is preserved as printed and the row is flagged
in the warnings column so that zeros can be excluded as appropriate.

## Counties listed twice

A few reports cover two separate incidents and print the whole county
list once for each, giving a different per capita figure each time. Both
figures are returned as separate rows, since neither supersedes the
other, and both rows are flagged in the warnings column. The report and
county together are therefore not inherently a unique key (though in
most cases they are).

## Examples

``` r
if (FALSE) { # \dontrun{
get_preliminary_damage_assessments() %>%
  transform_pda_counties()
} # }
```
