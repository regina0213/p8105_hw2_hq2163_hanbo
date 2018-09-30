p8105\_hw2\_hq2163\_hanbo
================
Hanbo Qiu
September 30, 2018

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.0.0     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.6
    ## v tidyr   0.8.1     v stringr 1.3.1
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts ------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

Import and clean NYC Transit csv files
--------------------------------------

Import my first csv (NYC\_Transit\_Subway\_Entrance\_And\_Exit\_Data.csv) and clean it.

``` r
nyc_sub  = read_csv(file = "./data/NYC_Transit_Subway_Entrance_And_Exit_Data.csv") %>% 
  janitor::clean_names() %>% 
  select(line:entry, vending, ada) %>% 
  mutate(entry = recode(entry, "YES" = TRUE, "NO" = FALSE)
  )
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   `Station Latitude` = col_double(),
    ##   `Station Longitude` = col_double(),
    ##   Route8 = col_integer(),
    ##   Route9 = col_integer(),
    ##   Route10 = col_integer(),
    ##   Route11 = col_integer(),
    ##   ADA = col_logical(),
    ##   `Free Crossover` = col_logical(),
    ##   `Entrance Latitude` = col_double(),
    ##   `Entrance Longitude` = col_double()
    ## )

    ## See spec(...) for full column specifications.

Write a short paragraph about this dataset
------------------------------------------

The original dataset contains variables related to subway station name, location and each entrance and exit for each subway station in NYC. I import the dataset, convert column names to lower snake case, retain column including "line, station, name, station latitude/longitude, routes served, entry, vending, entrance type, and ADA compliance" and convert the entry variable from character to a logical variable.The dimension of the dataset is 1868\*19. It is a tidy dataset.
