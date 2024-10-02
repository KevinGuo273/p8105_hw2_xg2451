p8105_hw2_xg2451
================
Xuanyu Guo
2024-09-27

# Problem1

``` r
library(dplyr)
library(tidyr)

# Load data
data <- read.csv("NYC_Transit_Subway_Entrance_And_Exit_Data.csv")

# Select and clean the required columns
clean_data <- data %>%
  select(Line, Station.Name, Station.Latitude, Station.Longitude,
         Route1:Route11, Entry, Vending, Entrance.Type, ADA) %>%
  mutate(Entry = ifelse(Entry == "YES", TRUE, FALSE))
```

``` r
# Check dimensions and print a summary
nrow(clean_data)
```

    ## [1] 1868

``` r
ncol(clean_data)
```

    ## [1] 19

The dataset contains variables related to NYC subway stations, including
line, station name, station latitude and longitude, up to 11 routes
served by each station, whether the entrance allows entry, whether there
is a vending machine, the type of entrance, and ADA compliance. The data
cleaning involved retaining relevant columns and converting the Entry
variable from a character string to a logical variable. The resulting
dataset has dimensions indicating the number of 1868 rows and 19
columns. The data structure has been arranged to meet the principles of
tidy data.

``` r
# Calculate the number of distinct stations
distinct_stations <- clean_data %>%
  distinct(Line, Station.Name) %>%
  nrow()
distinct_stations
```

    ## [1] 465

There are 465 distinct stations.

``` r
# Calculate how many stations are ADA compliant
ada_compliant_stations <- clean_data %>%
  filter(ADA == TRUE) %>%
  distinct(Line, Station.Name) %>%
  nrow()
ada_compliant_stations
```

    ## [1] 84

There are 84 ADA compliant stations.

``` r
# Proportion of station entrances/exits without vending that allow entrance
proportion_no_vending_entry <- clean_data %>%
  filter(Vending == "NO") %>%
  summarise(Proportion = mean(Entry))
proportion_no_vending_entry
```

    ##   Proportion
    ## 1  0.3770492

The proportion of station entrances / exits without vending allow
entrance is approximately 37.70%.

``` r
# Convert all Route types to character and all "" to NA
clean_data <- clean_data %>%
  mutate(across(starts_with("Route"), ~na_if(as.character(.), "")))
# Reformat data
reformat_data <- clean_data %>%
  pivot_longer(cols = Route1:Route11, values_to = "Route", names_to = "Route_Num", values_drop_na = TRUE)
```

``` r
# Separate route numbers into distinct variables and calculate A train stations
a_train_stations <- reformat_data %>%
  filter(Route == "A") %>%
  distinct(Line, Station.Name) %>%
  nrow()
a_train_stations
```

    ## [1] 60

``` r
# ADA compliant stations serving the A train
a_train_ada_compliant <- reformat_data %>%
  filter(Route == "A", ADA == TRUE) %>%
  distinct(Line, Station.Name) %>%
  nrow()
a_train_ada_compliant
```

    ## [1] 17

There are 60 distinct stations serve the A train, and 17 of them are ADA
compliant.

# Problem2

``` r
library(readxl)
# Read the data for Mr. Trash Wheel
mr_trash_wheel <- read_excel("Trash_Wheel_Collection_Data.xlsx", sheet = "Mr. Trash Wheel") %>%
  select(-c(...15, ...16)) %>%  # Assuming columns 15 and 16 are notes
  filter(!is.na(Dumpster)) %>%  # Remove rows without dumpster data
  rename( # omit columns containing notes
    Weight = "Weight (tons)",
    Volume = "Volume (cubic yards)",
    "Homes Powered" = "Homes Powered*"
    ) %>%
  # round and convert the result to an integer variable
  mutate(Year = as.numeric(Year),
         Sports_Balls = as.integer(round(`Sports Balls`)),
         Trash_Wheel = "Mr. Trash Wheel")


# Read the data for Professor Trash Wheel
prof_trash_wheel <- read_excel("Trash_Wheel_Collection_Data.xlsx", sheet = "Professor Trash Wheel") %>%
  filter(!is.na(Dumpster)) %>%  
  rename( 
    Weight = "Weight (tons)",
    Volume = "Volume (cubic yards)",
    "Homes Powered" = "Homes Powered*"
    ) %>%
  mutate(Trash_Wheel = "Professor Trash Wheel")

# Read the data for Gwynnda Trash Wheel
gwynnda_trash_wheel <- read_excel("Trash_Wheel_Collection_Data.xlsx", sheet = "Gwynnda Trash Wheel") %>%
  filter(!is.na(Dumpster)) %>%  
  rename(
    Weight = "Weight (tons)",
    Volume = "Volume (cubic yards)",
    "Homes Powered" = "Homes Powered*"
    ) %>%
  mutate(Trash_Wheel = "Gwynnda Trash Wheel")
```

``` r
combined_data <- bind_rows(mr_trash_wheel, prof_trash_wheel, gwynnda_trash_wheel)
                           
# Total observations in the dataset
total_observations <- nrow(combined_data)
total_observations
```

    ## [1] 845

``` r
# Total weight of trash collected by Professor Trash Wheel
total_weight_prof <- sum(prof_trash_wheel$Weight, na.rm = TRUE)
total_weight_prof
```

    ## [1] 216.26

``` r
# Total number of cigarette butts collected by Gwynnda in June 2022
cigarette_butts_june2022 <- sum(gwynnda_trash_wheel$`Cigarette Butts`[gwynnda_trash_wheel$Month == "June" & gwynnda_trash_wheel$Year == 2022], na.rm = TRUE)
cigarette_butts_june2022
```

    ## [1] 18120

The combined dataset for Mr. Trash Wheel, Professor Trash Wheel, and
Gwynnda consists of 845 observations, capturing various metrics of waste
collected from Baltimore’s waterways. For instance, key variables
include the weight of trash in tons and the number of specific waste
items like cigarette butts and sports balls. In total, Professor Trash
Wheel collected 216.26 tons of trash. In June 2022, Gwynnda collected
18120 cigarette butts, indicating the substantial impact these devices
have on reducing aquatic litter.

# Problem3

``` r
library(readr)
dat_bakers = read_csv("./gbb_datasets/bakers.csv") %>% 
  janitor::clean_names() %>% 
  separate(
    baker_name, 
    into = c("baker", "baker_last_name"), 
    sep = " "
  ) 
```

    ## Rows: 120 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): Baker Name, Baker Occupation, Hometown
    ## dbl (2): Series, Baker Age
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
dat_bakes = read_csv("./gbb_datasets/bakes.csv") %>% 
  janitor::clean_names() %>% 
  mutate(
    baker = ifelse(baker == "\"Jo\"", "Jo", baker)
  )
```

    ## Rows: 548 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): Baker, Signature Bake, Show Stopper
    ## dbl (2): Series, Episode
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
dat_results = read_csv("./gbb_datasets/results.csv", skip = 2) %>% 
  janitor::clean_names() %>% 
  filter(!is.na(result)) %>% 
  mutate(
    baker = ifelse(baker == "Joanne", "Jo", baker)
  )
```

    ## Rows: 1136 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): baker, result
    ## dbl (3): series, episode, technical
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# check 
anti_join(dat_bakers, dat_results)
```

    ## Joining with `by = join_by(baker, series)`

``` r
anti_join(dat_bakers, dat_bakes)
```

    ## Joining with `by = join_by(baker, series)`

``` r
# merge
dat_merge1 = left_join(dat_results, dat_bakes) 
```

    ## Joining with `by = join_by(series, episode, baker)`

``` r
dat_merge2 = left_join(dat_merge1, dat_bakers)
```

    ## Joining with `by = join_by(series, baker)`

``` r
# organize in meaningful orders
dat_merge = dat_merge2 %>% 
  distinct() %>% 
  select(series, episode, baker, signature_bake, technical, result, 
         show_stopper, everything())
```

``` r
# summary
skimr::skim(dat_merge)
```

|                                                  |           |
|:-------------------------------------------------|:----------|
| Name                                             | dat_merge |
| Number of rows                                   | 710       |
| Number of columns                                | 11        |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |           |
| Column type frequency:                           |           |
| character                                        | 7         |
| numeric                                          | 4         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |           |
| Group variables                                  | None      |

Data summary

**Variable type: character**

| skim_variable    | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:-----------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| baker            |         0 |          1.00 |   2 |  10 |     0 |      107 |          0 |
| signature_bake   |       162 |          0.77 |   8 | 123 |     0 |      545 |          0 |
| result           |         0 |          1.00 |   2 |  10 |     0 |        7 |          0 |
| show_stopper     |       164 |          0.77 |   3 | 200 |     0 |      529 |          0 |
| baker_last_name  |         0 |          1.00 |   3 |  17 |     0 |      117 |          0 |
| baker_occupation |         0 |          1.00 |   5 |  42 |     0 |      106 |          0 |
| hometown         |         0 |          1.00 |   5 |  41 |     0 |       96 |          0 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |  mean |    sd |  p0 | p25 | p50 | p75 | p100 | hist  |
|:--------------|----------:|--------------:|------:|------:|----:|----:|----:|----:|-----:|:------|
| series        |         0 |          1.00 |  5.85 |  2.72 |   1 |   4 |   6 |   8 |   10 | ▅▇▇▇▇ |
| episode       |         0 |          1.00 |  4.26 |  2.61 |   1 |   2 |   4 |   6 |   10 | ▇▆▅▃▂ |
| technical     |        14 |          0.98 |  4.84 |  2.98 |   1 |   2 |   4 |   7 |   13 | ▇▅▅▂▁ |
| baker_age     |         0 |          1.00 | 36.31 | 12.85 |  17 |  28 |  32 |  42 |   71 | ▅▇▃▁▂ |

``` r
# write csv
write_csv(dat_merge, "./gbb_datasets/merge_data.csv")
```

**Cleaning progress:**

- In dataset dat_bakers, the variable “baker_name” had different name
  and form from other two datasets, so separate this variable into two
  columns “baker” and “baker_last_name”.
- In dataset dat_bakes, “Jo” in variable “baker” had a quotation marks,
  so remove it.
- In dataset dat_results, omit the first two useless rows and change
  “Joanne” in column “baker” to “Jo”.

**Briefly Discussion:**

There are 710 rows and 11 columns, including 7 character variables (eg.
baker, signature_bake, result) and 4 numeric variables (eg. series,
episode, technical). Variables “signature_bake”, “show_stopper” and
“technical” have missing values.

**Star bakers / Winners:**

``` r
star_baker = dat_merge %>% 
  filter(series >= 5 & series <= 10 & result %in% c("STAR BAKER", "WINNER")) %>% 
  select(series, episode, result, baker) %>% 
  arrange(series, episode)
star_baker
```

    ## # A tibble: 60 × 4
    ##    series episode result     baker  
    ##     <dbl>   <dbl> <chr>      <chr>  
    ##  1      5       1 STAR BAKER Nancy  
    ##  2      5       2 STAR BAKER Richard
    ##  3      5       3 STAR BAKER Luis   
    ##  4      5       4 STAR BAKER Richard
    ##  5      5       5 STAR BAKER Kate   
    ##  6      5       6 STAR BAKER Chetna 
    ##  7      5       7 STAR BAKER Richard
    ##  8      5       8 STAR BAKER Richard
    ##  9      5       9 STAR BAKER Richard
    ## 10      5      10 WINNER     Nancy  
    ## # ℹ 50 more rows

Overall Winners in every series:

- Richard obtained 5 star baker in series 5.
- Nadiya obtained 4 star baker in series 6.
- Candice obtained 4 star baker in series 7.
- Steven and Sophie both obtained 3 star baker in series 8.
- Rahul obtained 3 star baker in series 9.
- Steph obtained 4 star baker in series 10.
- **It is not surprising that they are winners.**

**Viewers:**

``` r
dat_viewers = read_csv("./gbb_datasets/viewers.csv") %>% 
  janitor::clean_names()
```

    ## Rows: 10 Columns: 11
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (11): Episode, Series 1, Series 2, Series 3, Series 4, Series 5, Series ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(dat_viewers, 10) #the first 10 rows
```

    ## # A tibble: 10 × 11
    ##    episode series_1 series_2 series_3 series_4 series_5 series_6 series_7
    ##      <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
    ##  1       1     2.24     3.1      3.85     6.6      8.51     11.6     13.6
    ##  2       2     3        3.53     4.6      6.65     8.79     11.6     13.4
    ##  3       3     3        3.82     4.53     7.17     9.28     12.0     13.0
    ##  4       4     2.6      3.6      4.71     6.82    10.2      12.4     13.3
    ##  5       5     3.03     3.83     4.61     6.95     9.95     12.4     13.1
    ##  6       6     2.75     4.25     4.82     7.32    10.1      12       13.1
    ##  7       7    NA        4.42     5.1      7.76    10.3      12.4     13.4
    ##  8       8    NA        5.06     5.35     7.41     9.02     11.1     13.3
    ##  9       9    NA       NA        5.7      7.41    10.7      12.6     13.4
    ## 10      10    NA       NA        6.74     9.45    13.5      15.0     15.9
    ## # ℹ 3 more variables: series_8 <dbl>, series_9 <dbl>, series_10 <dbl>

``` r
# average viewership in Season 1
average_1 = mean(dat_viewers$series_1, na.rm = TRUE) 
average_1
```

    ## [1] 2.77

``` r
# Season 5
average_5 = mean(dat_viewers$series_5, na.rm = TRUE)
average_5
```

    ## [1] 10.0393

The average viewership in Season 1 is 2.77.  
The average viewership in Season 5 is 10.04.
