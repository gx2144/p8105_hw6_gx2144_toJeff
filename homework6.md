homework6
================
Guangling Xu
2019/11/24

-----

``` r
library(tidyverse)
```

    ## -- Attaching packages ----------------------- tidyverse 1.3.0 --

    ## √ ggplot2 3.2.1     √ purrr   0.3.3
    ## √ tibble  2.1.3     √ dplyr   0.8.3
    ## √ tidyr   1.0.0     √ stringr 1.4.0
    ## √ readr   1.3.1     √ forcats 0.4.0

    ## -- Conflicts -------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
knitr::opts_chunk$set(
    echo = TRUE,
    warning = FALSE,
    fig.width = 8, 
  fig.height = 6,
  out.width = "90%"
)
options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)
scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
theme_set(theme_minimal() + theme(legend.position = "bottom"))
```

### Data Cleaning

``` r
birthweight =
  read_csv("./data/birthweight.csv") %>% 
  janitor::clean_names() %>% 
  mutate(
    babysex = factor(babysex, labels = c("male", "female")),
    frace  = factor(frace, labels = c("White","Black","Asian","Puerto Rican","Other")),
    malform = factor(malform, labels = c("absent","present")),
    mrace = factor(mrace, labels = c("White","Black","Asian","Puerto Rican")),
    babysex = fct_infreq(babysex),
    frace = fct_infreq(frace),
    malform=  fct_infreq(malform),
    mrace = fct_infreq(mrace)
     )
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double()
    ## )

    ## See spec(...) for full column specifications.
