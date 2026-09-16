# Compare the Local PDF Cache Against the Number of Reports FEMA Lists

Compares the FEMA-reported number of reports to the number of PDF files
in `cache_directory`.

## Usage

``` r
check_pda_cache(cache_directory)
```

## Arguments

- cache_directory:

  The folder where
  [`scrape_pda_pdfs()`](https://UrbanInstitute.github.io/preliminary-damage-assessments/reference/scrape_pda_pdfs.md)
  writes PDFs.

## Value

A tibble with one row and three columns: `listed` (the count FEMA
shows), `cached` (PDF files in `cache_directory`), and `difference`
(`listed - cached`; positive means the cache probably has reports
missing).

## Details

The two numbers are not guaranteed to match exactly even when the cache
is complete. FEMA's count is the number of entries in the listing, while
the cache holds one file per unique PDF. They can differ when two
listing entries link to the same PDF, when a report was removed from the
site but its file is still on disk, or when a download failed. Treat a
match as reassuring and a large mismatch as a reason to run
[`scrape_pda_pdfs()`](https://UrbanInstitute.github.io/preliminary-damage-assessments/reference/scrape_pda_pdfs.md).

## Examples

``` r
if (FALSE) { # \dontrun{
check_pda_cache(cache_directory = file.path("data", "pdfs"))
} # }
```
