# Validating the Preliminary Damage Assessment Fields

## Load data

Note that this relies on a cached dataset; refer to the README for
creating such data from scratch.

``` r

pdas = get_preliminary_damage_assessments(
  file_path = file.path("..", "data", "pdas.csv"),
  use_cache = TRUE)
  
pdas = pdas %>%
  ## many records precede 2007/2008, which is the first year when PDAs are available /
  ## included in this dataset -- these pre-2007 records have only values direct from
  ## authoritative FEMA datasets
  filter(fema_decision_year > 2006)
```

## Data checks

### What time period do PDAs cover?

``` r

pdas %>%
  summarize(
    earliest = min(pda_date_determined, na.rm = TRUE),
    latest = max(pda_date_determined, na.rm = TRUE),
    missing = sum(is.na(pda_date_determined)))
#> # A tibble: 1 × 3
#>   earliest   latest     missing
#>   <date>     <date>       <int>
#> 1 2007-10-02 2026-04-23     116

## the determination precedes the decision it feeds
pdas %>%
  count(
    determined_before_decision =
      pda_date_determined <= fema_decision_date)
#> # A tibble: 3 × 2
#>   determined_before_decision     n
#>   <lgl>                      <int>
#> 1 FALSE                          1
#> 2 TRUE                        1368
#> 3 NA                           116

## there is only one day of difference, and the join is on the disaster number
pdas %>%
  filter(pda_date_determined > fema_decision_date) %>%
  select(fema_decision_date, pda_date_determined, pda_warnings)
#> # A tibble: 1 × 3
#>   fema_decision_date pda_date_determined pda_warnings
#>   <date>             <date>              <chr>       
#> 1 2011-08-12         2011-08-13          <NA>
```

### How does PDA match coverage look, relative to the universe of approvals and denials?

``` r

pdas %>% count(fema_decision, pda_matched)
#> # A tibble: 4 × 3
#>   fema_decision pda_matched     n
#>   <chr>         <lgl>       <int>
#> 1 Approved      FALSE          81
#> 2 Approved      TRUE         1178
#> 3 Denied        FALSE          35
#> 4 Denied        TRUE          191

## pdas only exist for ~2008 onward
pdas %>%
  filter(fema_decision_year > 2007) %>%
  count(fema_decision, pda_matched)
#> # A tibble: 4 × 3
#>   fema_decision pda_matched     n
#>   <chr>         <lgl>       <int>
#> 1 Approved      FALSE          25
#> 2 Approved      TRUE         1171
#> 3 Denied        FALSE          30
#> 4 Denied        TRUE          189

## and coverage for the most recent events
## (2026, as of time of writing) is relatively poor
pdas %>%
  filter(
    fema_decision_year > 2007,
    fema_decision_year < 2026) %>%
  count(fema_decision, pda_matched)
#> # A tibble: 4 × 3
#>   fema_decision pda_matched     n
#>   <chr>         <lgl>       <int>
#> 1 Approved      FALSE           3
#> 2 Approved      TRUE         1158
#> 3 Denied        FALSE          15
#> 4 Denied        TRUE          182
```

### How accurately are PDAs matched to their corresponding records from authoritative FEMA sources?

``` r

pdas %>%
  filter(pda_matched) %>%
  mutate(match_type = str_extract(pda_match_quality, "^[a-z]+")) %>%
  count(fema_decision, match_type)
#> # A tibble: 4 × 3
#>   fema_decision match_type      n
#>   <chr>         <chr>       <int>
#> 1 Approved      exact        1178
#> 2 Denied        approximate     4
#> 3 Denied        exact         185
#> 4 Denied        manual          2
```

### Do PDA-derived request decisions align with FEMA-reported decisions?

``` r

pdas %>% 
  filter(pda_matched) %>%
  count(fema_decision, pda_decision)
#> # A tibble: 4 × 3
#>   fema_decision pda_decision        n
#>   <chr>         <chr>           <int>
#> 1 Approved      appeal_approved    21
#> 2 Approved      approved         1157
#> 3 Denied        appeal_denial      92
#> 4 Denied        denial             99
```

### For less high-precision matches, do dates and event titles match?

``` r

## even for events not joined on the exact disaster number, event language aligns
## with fema-reported event titles
pdas %>%
  filter(
    pda_matched,
    !str_detect(pda_match_quality, "exact") | is.na(pda_match_quality)) %>%
  select(fema_declaration_title, pda_text, matches("date")) %>%
  select(-fema_declaration_request_date) %>%
  mutate(
    pda_text = pda_text %>% 
      str_sub(1, 100) %>% str_remove("Preliminary Damage Assessment Report")) %>%
  print(width = Inf)
#> # A tibble: 6 × 4
#>   fema_declaration_title                     
#>   <chr>                                      
#> 1 California Extreme Heat Event and Wildfires
#> 2 Severe Weather 11/4/2022                   
#> 3 Severe Weather                             
#> 4 OR_Severe Winter Storm                     
#> 5 SD - Severe Storms 07/04/2018              
#> 6 OST severe storm 07/27/2018                
#>   pda_text                                                          
#>   <chr>                                                             
#> 1 "  California – Heat Dome and resulting Wildfires Denial of Appea"
#> 2 "  Choctaw Nation of Oklahoma – Severe Storms and Tornadoes Denia"
#> 3 "  Texas – Severe Storms and Tornadoes Denial Denied on March 15,"
#> 4 "  Oregon – Severe Winter Storm, Straight-line Winds, Flooding, L"
#> 5 "  Cheyenne River Sioux Tribe – Severe Storms and Straight-line W"
#> 6 "  Oglala Sioux Tribe – Severe Storms, Tornadoes, and Straight-li"
#>   fema_decision_date pda_date_determined
#>   <date>             <date>             
#> 1 2023-05-16         2023-05-09         
#> 2 2023-05-18         2023-05-17         
#> 3 2023-03-16         2023-03-15         
#> 4 2023-04-05         2023-04-04         
#> 5 2018-09-18         2018-09-18         
#> 6 2018-09-18         2018-09-18
```

### Tribal-led declaration requests have PDAs that are formatted differently. Are these distinguished correctly?

``` r

pdas %>% 
  filter(pda_matched) %>%
  count(pda_tribal_flag)
#> # A tibble: 2 × 2
#>   pda_tribal_flag     n
#>             <dbl> <int>
#> 1               0  1290
#> 2               1    79

pdas %>%
  filter(pda_tribal_flag == 1) %>%
  count(pda_tribal_name, pda_tribal_fips) %>%
  arrange(pda_tribal_name) %>%
  print(n = Inf)
#> # A tibble: 46 × 3
#>    pda_tribal_name                                         pda_tribal_fips     n
#>    <chr>                                                   <chr>           <int>
#>  1 Agua Caliente Band of Cahuilla Indians                  0020R               1
#>  2 Assiniboine and Sioux Tribes of the Fort Peck Indian R… 1250R               2
#>  3 Bad River Band of Lake Superior Tribe of Chippewa Indi… 0140R               1
#>  4 Bear River Band of the Rohnerville Rancheria            3220T               1
#>  5 Burns Paiute Tribe                                      0400R               1
#>  6 Cahuilla Band of Indians                                0435R               1
#>  7 Cheyenne River Sioux Tribe                              0605R               2
#>  8 Chickasaw Nation                                        5580R               1
#>  9 Choctaw Nation of Oklahoma                              5590R               1
#> 10 Confederated Tribes and Bands of the Yakama Nation      4690R               1
#> 11 Confederated Tribes of the Colville Reservation         0760R               4
#> 12 Crow Tribe of Montana                                   0845R               2
#> 13 Eastern Band of Cherokee Indians                        0990R               1
#> 14 Havasupai Tribe                                         1440R               3
#> 15 Hoopa Valley Tribe                                      1490R               3
#> 16 Karuk Tribe                                             1750R               1
#> 17 La Jolla Band of Luiseño Indians                        1850R               2
#> 18 Leech Lake Band of Ojibwe                               1940R               1
#> 19 Los Coyotes Band of Cahuilla-Cupeño Indians             1995R               1
#> 20 Morongo Band of Mission Indians                         2360R               1
#> 21 Muscogee (Creek) Nation                                 5620R               3
#> 22 Native Village of Kipnuk                                6750R               1
#> 23 Native Village of Kivalina                              6755R               1
#> 24 Native Village of Kwigillingok                          6840R               1
#> 25 Navajo Nation                                           2430R               5
#> 26 Newtok Village                                          7055R               1
#> 27 Oglala Sioux Tribe                                      2810R               4
#> 28 Poarch Band of Creek Indians                            2865R               1
#> 29 Ponca Tribe of Nebraska                                 2900T               1
#> 30 Pueblo of Acoma                                         0010R               1
#> 31 Red Lake Band of Chippewa Indians                       3100R               1
#> 32 Resighini Rancheria                                     3145R               1
#> 33 Rosebud Sioux Tribe                                     3235R               3
#> 34 Sac and Fox Tribe of the Mississippi in Iowa            3280R               2
#> 35 Saint Regis Mohawk Tribe                                3320R               1
#> 36 Salt River Pima-Maricopa Indian Community               3340R               1
#> 37 San Carlos Apache Tribe                                 3355R               2
#> 38 Santa Clara Pueblo                                      3495R               3
#> 39 Seminole Tribe of Florida                               3665T               4
#> 40 Sisseton-Wahpeton Oyate                                 1860R               2
#> 41 Soboba Band of Luiseño Indians                          3870R               4
#> 42 Spokane Tribe                                           3940R               1
#> 43 Standing Rock Sioux Tribe                               3970R               1
#> 44 Sycuan Band of the Kumeyaay Nation                      4090R               1
#> 45 Tohono O'odham Nation                                   4200R               1
#> 46 Wrangell Cooperative Association                        7755R               1

## tribal-led PDAs typically reference the tribal leader who requested the declaration
## this should return no observations, because all matching reports should be flagged as tribal
pdas %>%
  filter(pda_tribal_flag == 0, str_detect(pda_text, "Chairman|Chairwoman|Chairperson|Council President|Chief")) %>%
  head() %>%
  pull(pda_text)
#> character(0)
```

### Do we accurately/consistently derive the referenced hazards in the PDAs? These are important because we use these to disambiguate matches between PDAs and authoritative FEMA denial records.

``` r

pdas %>%
  filter(pda_matched) %>%
  separate_longer_delim(pda_hazards, delim = "; ") %>%
  count(pda_hazards, sort = TRUE) %>%
  print(n = Inf)
#> # A tibble: 14 × 2
#>    pda_hazards           n
#>    <chr>             <int>
#>  1 severe storm        778
#>  2 flooding            683
#>  3 tornado             326
#>  4 winter storm        238
#>  5 landslide           137
#>  6 hurricane           112
#>  7 tropical storm       65
#>  8 wildfire             65
#>  9 pandemic             59
#> 10 <NA>                 12
#> 11 earthquake           11
#> 12 drought               4
#> 13 tsunami               4
#> 14 volcanic eruption     3

pdas %>%
  filter(pda_matched) %>%
  separate_longer_delim(pda_hazards, delim = "; ") %>%
  filter(is.na(pda_hazards)) %>%
  select(fema_decision_date, fema_declaration_title, pda_declaration_title)
#> # A tibble: 12 × 3
#>    fema_decision_date fema_declaration_title               pda_declaration_title
#>    <date>             <chr>                                <chr>                
#>  1 2013-08-02         EXPLOSION                            Explosion            
#>  2 2008-07-11         OR-Fishery Closure-06-06-2008        Closure of the Salmo…
#>  3 2013-06-10         Explosion                            Explosion            
#>  4 2013-08-05         San Carlos Apache Tribe Power Outage San Carlos Apache Tr…
#>  5 2014-05-20         Chemical Spill                       Chemical Spill       
#>  6 2015-07-29         R3_Baltimore_City_Unrest             Civil Unrest         
#>  7 2015-07-07         Iowa Highly Pathogenic Avian Influe… Highly Pathogenic Av…
#>  8 2016-01-22         Contaminated Water                   Contaminated Water   
#>  9 2017-05-18         ND-Dakota Access Pipeline            Civil Unrest         
#> 10 2020-08-18         MN Fires due to Civil Unrest         Civil Unrest         
#> 11 2025-01-01         Ohio Train Derailment, East Palesti… Train Derailment     
#> 12 2024-03-15         AK_Permafrost Degradation            Building Collapse

## virtually all joined pdas have a shared hazard(s) with the matching FEMA event
pdas %>%
  filter(!is.na(fema_hazards), !is.na(pda_hazards)) %>%
  mutate(
    shared_hazards = map2_int(
      str_split(pda_hazards, "; "),
      str_split(fema_hazards, "; "),
      ~ length(intersect(.x, .y)))) %>%
  count(any_shared_hazard = shared_hazards > 0)
#> # A tibble: 2 × 2
#>   any_shared_hazard     n
#>   <lgl>             <int>
#> 1 FALSE                 1
#> 2 TRUE                176
```

### Do PA requests derived from PDAs align with FEMA-reported values?

``` r

pdas %>% 
  filter(pda_matched) %>%
  count(pda_pa_requested)
#> # A tibble: 2 × 2
#>   pda_pa_requested     n
#>   <lgl>            <int>
#> 1 FALSE              162
#> 2 TRUE              1207

## very small shares of matched records disagree--users should review the records in question
## and make their own determinations about which value they believe to be correct
pdas %>%
  filter(fema_decision == "Denied", pda_matched) %>%
  count(fema_pa_requested, pda_pa_requested)
#> # A tibble: 4 × 3
#>   fema_pa_requested pda_pa_requested     n
#>   <lgl>             <lgl>            <int>
#> 1 FALSE             FALSE               70
#> 2 FALSE             TRUE                 4
#> 3 TRUE              FALSE                4
#> 4 TRUE              TRUE               113
```

### Do Individual Assistance (IA) requests derived from PDAs align with FEMA-reported values?

Note that differences between IA and Individuals and Households Program
(IHP)–a subset of IA–explain some divergence.

``` r

pdas %>% 
  filter(pda_matched) %>%
  count(pda_ia_requested)
#> # A tibble: 2 × 2
#>   pda_ia_requested     n
#>   <lgl>            <int>
#> 1 FALSE              701
#> 2 TRUE               668

## very small shares of matched records disagree--users should review the records in question
## and make their own determinations about which value they believe to be correct
pdas %>%
  filter(fema_decision == "Denied", pda_matched) %>%
  count(fema_ihp_requested, pda_ia_requested)
#> # A tibble: 3 × 3
#>   fema_ihp_requested pda_ia_requested     n
#>   <lgl>              <lgl>            <int>
#> 1 FALSE              FALSE               79
#> 2 FALSE              TRUE                 5
#> 3 TRUE               TRUE               107
```

### Do we have unexpected missingness for PA cost estimates?

``` r

pdas %>%
  filter(pda_pa_requested == 1) %>%
  summarize(
    nonmissing = sum(!is.na(pda_pa_cost_estimate_total)),
    missing_no_preemptive_declaration = sum(
      is.na(pda_pa_cost_estimate_total) & pda_pa_preemptive_declaration == 0),
    minimum = min(pda_pa_cost_estimate_total, na.rm = TRUE),
    median = median(pda_pa_cost_estimate_total, na.rm = TRUE),
    maximum = max(pda_pa_cost_estimate_total, na.rm = TRUE))
#> # A tibble: 1 × 5
#>   nonmissing missing_no_preemptive_declaration minimum   median   maximum
#>        <int>                             <int>   <dbl>    <dbl>     <dbl>
#> 1       1004                                24       0 8594382. 537119780

## some (few) records simply have no listed PA cost estimates, despite
## requesting PA -- as evidenced by matched "Public Assistance" terms in the PDA
## text (code below). this can be due to, e.g., ongoing PDA activities, or potentially to cases
## that do not reference preemptive declarations but where total damages are nonetheless
## assumed to far surpass the statewide per capita impact threshold
pdas %>%
  filter(
    pda_pa_requested == 1,
    is.na(pda_pa_cost_estimate_total),
    pda_pa_preemptive_declaration == 0) %>%
  filter(
    !str_detect(pda_text, 
    "Public Assistance (for|and)|Public Assistance (C|c)ategories|Public Assistance program|Public Assistance Category")) %>%
  pull(pda_text)
#> character(0)
```

### There should be two different indicator values per calendar year–one for each fiscal year.

Users can consider imputing the presumably-applicable value for those
limited cases where the value in the PDA report does not align with the
statutorily-applicable value.

``` r

## the statutory threshold is uniform within a federal fiscal year, which runs
## from October through September
pdas %>%
  filter(pda_matched) %>%
  mutate(
    fiscal_year = lubridate::year(pda_date_determined) +
      if_else(lubridate::month(pda_date_determined) >= 10, 1, 0)) %>%
  count(fiscal_year, pda_pa_per_capita_impact_indicator_statewide) %>%
  arrange(fiscal_year) %>%
  print(n = Inf)
#> # A tibble: 30 × 3
#>    fiscal_year pda_pa_per_capita_impact_indicator_statewide     n
#>          <dbl>                                        <dbl> <int>
#>  1        2008                                         1.24    77
#>  2        2009                                         1.31    67
#>  3        2010                                         1.29    88
#>  4        2010                                         1.31     2
#>  5        2011                                         1.29     2
#>  6        2011                                         1.3    109
#>  7        2012                                         1.3      1
#>  8        2012                                         1.35    55
#>  9        2013                                         1.37    71
#> 10        2014                                         1.39    59
#> 11        2015                                         1.39     1
#> 12        2015                                         1.41    52
#> 13        2016                                         1.41    57
#> 14        2017                                         1.43    69
#> 15        2018                                         1.46    64
#> 16        2019                                         1.46     1
#> 17        2019                                         1.5     73
#> 18        2020                                         1.53   110
#> 19        2020                                         1.66     1
#> 20        2020                                         2.48     1
#> 21        2021                                         1.55    65
#> 22        2022                                         1.63    63
#> 23        2023                                         1.77    79
#> 24        2024                                         1.77    17
#> 25        2024                                         1.84    84
#> 26        2025                                         1.77     1
#> 27        2025                                         1.84    21
#> 28        2025                                         1.89    50
#> 29        2026                                         1.89    13
#> 30        2026                                         1.94    16
```

### How well can we create a derived dataset where each record represents a county-declaration observation?

``` r

pdas %>%
  filter(!is.na(pda_pa_per_capita_impact_countywide)) %>%
  slice_sample(n = 3) %>%
  pull(pda_pa_per_capita_impact_countywide) %>%
  str_squish()
#> [1] "Clay County 7.00 Douglas County 21.73 Jackson County 2.13"                                                                                                                                                                                                                                                                                                                    
#> [2] "Cameron County 4.41 Cottle County 79.50 Floyd County 263.36 Foard County 139.68 Garza County 137.84 Hidalgo County 18.37 Jim Hogg County 109.87 Jim Wells County 9.88 Lamb County 46.84 Lubbock County 4.13 Lynn County 16.81 Maverick County 15.55 Motley County 366.31 Starr County 30.65 Terry County 57.35 Webb County 8.67 Willacy County 56.95 and Zapata County 32.93."
#> [3] "Barnes Franklin County 10.83 Kennebec County 4.70 Knox County 6.52 Lincoln County 4.46 Oxford County 5.67 Sagadahoc County 8.57 Somerset County 6.36 and Waldo County 8.42."

counties = pdas %>% transform_pda_counties()

counties %>%
  filter(pda_matched) %>%
  count(is.na(pda_county_geoid))
#> # A tibble: 2 × 2
#>   `is.na(pda_county_geoid)`     n
#>   <lgl>                     <int>
#> 1 FALSE                     11004
#> 2 TRUE                        170

counties %>% 
  mutate(missing_geoid = is.na(pda_county_geoid)) %>%
  count(missing_geoid, pda_geography_type, sort = TRUE) %>%
  filter(missing_geoid)
#> # A tibble: 8 × 3
#>   missing_geoid pda_geography_type     n
#>   <lgl>         <chr>              <int>
#> 1 TRUE          tribal entity         64
#> 2 TRUE          county                53
#> 3 TRUE          education area        35
#> 4 TRUE          unrecognized          12
#> 5 TRUE          independent city       2
#> 6 TRUE          island                 2
#> 7 TRUE          borough                1
#> 8 TRUE          parish                 1

counties %>%
  mutate(missing_geoid = is.na(pda_county_geoid)) %>%
  filter(pda_matched, missing_geoid) %>%
  count(fema_state_name, pda_county_name_reported, pda_geography_type, sort = TRUE) %>%
  print(n = 10)
#> # A tibble: 148 × 4
#>    fema_state_name pda_county_name_reported pda_geography_type     n
#>    <chr>           <chr>                    <chr>              <int>
#>  1 Alaska          Lower Yukon REAA         education area         4
#>  2 Alaska          Kuspuk REAA              education area         3
#>  3 Alaska          Lower Kuskokwim REAA     education area         3
#>  4 Alaska          Yukon Flats REAA         education area         3
#>  5 Alaska          Alaska Gateway REAA      education area         2
#>  6 Alaska          Copper River REAA        education area         2
#>  7 Alaska          Yukon-Koyukuk REAA       education area         2
#>  8 Idaho           Nez Perce Tribe          tribal entity          2
#>  9 Kentucky        Breckenridge County      county                 2
#> 10 Kentucky        Elliot County            county                 2
#> # ℹ 138 more rows
```

### Are percentage values reasonably distributed?

``` r

pdas %>%
  select(ends_with("_percent")) %>%
  pivot_longer(everything(), names_to = "column", values_to = "value") %>%
  filter(!is.na(value)) %>%
  summarize(
    .by = column,
    records = n(),
    minimum = min(value),
    median = median(value),
    maximum = max(value)) %>%
  print(n = Inf)
#> # A tibble: 9 × 5
#>   column                                          records minimum median maximum
#>   <chr>                                             <int>   <dbl>  <dbl>   <dbl>
#> 1 pda_ia_residences_insured_total_percent             469     0     30     100  
#> 2 pda_ia_households_poverty_percent                   484     0     22.2   100  
#> 3 pda_ia_households_owner_percent                     261     1.6   72.9   100  
#> 4 pda_ia_pre_disaster_unemployment_percent            203     2.1    5.7    29.8
#> 5 pda_ia_65plus_percent                               224     4.1   17.4    36.8
#> 6 pda_ia_disability_percent                           216     3.8   15.0    59  
#> 7 pda_ia_residences_insured_flood_percent              91     0     12      73.2
#> 8 pda_ia_18below_percent                              207     5.1   22.1    79.1
#> 9 pda_ia_population_other_government_assistance_…      60     0.2   11.6    46.5
```

### Do we have (m)any warnings about potential quality issues?

``` r

pdas %>%
  summarize(
    records = n(),
    flagged = sum(!is.na(pda_warnings)))
#> # A tibble: 1 × 2
#>   records flagged
#>     <int>   <int>
#> 1    1485      89

## outlier and equals-zero warnings are dropped -- these aren't inherently issues, just flags
## for anomalous values 
pdas %>%
  separate_longer_delim(pda_warnings, delim = "; ") %>%
  filter(!is.na(pda_warnings), !str_detect(pda_warnings, "100 times|exactly zero")) %>%
  count(pda_warnings, sort = TRUE)
#> # A tibble: 6 × 2
#>   pda_warnings                                                                 n
#>   <chr>                                                                    <int>
#> 1 the report disagrees with itself about whether ia was requested--the fl…    24
#> 2 the report disagrees with itself about whether pa was requested--the fl…    16
#> 3 the four damage categories sum to more than the stated total of impacte…    11
#> 4 pa was requested--the flag was settled from the opening narrative            1
#> 5 the report carries values for a program that ia_requested records as no…     1
#> 6 the report disagrees with itself about whether ia                            1
```
