# Download Preliminary Damage Assessment (PDA) PDF Reports to Disk

Downloads every PDA report that FEMA publishes into a local directory so
that
[`get_preliminary_damage_assessments()`](https://UrbanInstitute.github.io/preliminarydamages/reference/get_preliminary_damage_assessments.md)
has a complete and current set of source documents to parse. Run this
before regenerating the dataset;
[`get_preliminary_damage_assessments()`](https://UrbanInstitute.github.io/preliminarydamages/reference/get_preliminary_damage_assessments.md)
parses whatever is already on disk and never fetches anything itself.

## Usage

``` r
scrape_pda_pdfs(
  cache_directory,
  pages = NULL,
  max_pages = 200,
  attempts_per_page = 5,
  delay_seconds = 2,
  quiet = FALSE
)
```

## Arguments

- cache_directory:

  The folder where scraped PDFs are written.

- pages:

  Which listing pages to read, as a numeric vector. The default `NULL`
  walks the whole listing until a page returns no links. FEMA lists
  newest reports first, so `pages = c(0:5)` is often sufficient and
  faster.

- max_pages:

  A guard against an unbounded walk if the listing ever stops returning
  empty pages. Users should generally leave this as-is; default = 200.

- attempts_per_page:

  How many times to try a listing page before treating it as a failure.

- delay_seconds:

  Seconds to pause between searching listing pages. Most users should
  leave this as-is; shortening this delay can lead to an IP block.

- quiet:

  Suppress progress messages? Progress is reported by default. Warnings
  are always raised, regardless of this setting.

## Value

Invisibly, a tibble with one row per report found on the site,
containing `url`, `destination_file`, and `status` (`"cached"`,
`"downloaded"`, or `"failed"`). Called for its side effect; PDFs are
written to `cache_directory`.

## Details

Walks every page of FEMA's PDA report listing at
https://www.fema.gov/disaster/how-declared/preliminary-damage-assessments/reports
and downloads any report not already present in `cache_directory`.

## Examples

``` r
if (FALSE) { # \dontrun{
## refresh the local archive, then rebuild the dataset from it
scrape_pda_pdfs(cache_directory = file.path("data", "pdfs"))
get_preliminary_damage_assessments(
  file_path = file.path("data", "pdas.csv"),
  directory_path = file.path("data", "pdfs"),
  use_cache = FALSE)
} # }
```
