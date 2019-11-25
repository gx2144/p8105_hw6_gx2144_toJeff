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

### Model Building

First we hypothesize that birthweight was affected by all the predcitors
given by the dataset.

``` r
reg_lbt_m = lm(bwt ~ babysex + bhead + blength + delwt + fincome + frace + gaweeks + menarche + momage + mheight + mrace + parity + pnumlbw + pnumsga + ppbmi + ppwt + smoken + wtgain, data = birthweight)  

summary(reg_lbt_m)
```

    ## 
    ## Call:
    ## lm(formula = bwt ~ babysex + bhead + blength + delwt + fincome + 
    ##     frace + gaweeks + menarche + momage + mheight + mrace + parity + 
    ##     pnumlbw + pnumsga + ppbmi + ppwt + smoken + wtgain, data = birthweight)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1097.66  -184.94    -3.24   173.08  2343.95 
    ## 
    ## Coefficients: (3 not defined because of singularities)
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       -6264.3404   660.2824  -9.487  < 2e-16 ***
    ## babysexfemale        28.6899     8.4633   3.390 0.000705 ***
    ## bhead               130.7818     3.4518  37.888  < 2e-16 ***
    ## blength              74.9491     2.0212  37.082  < 2e-16 ***
    ## delwt                 4.1026     0.3946  10.398  < 2e-16 ***
    ## fincome               0.2893     0.1795   1.612 0.107079    
    ## fraceBlack           14.3004    46.1444   0.310 0.756647    
    ## fracePuerto Rican   -47.0259    44.6726  -1.053 0.292548    
    ## fraceAsian           21.1932    69.2874   0.306 0.759716    
    ## fraceOther            4.2345    74.0643   0.057 0.954410    
    ## gaweeks              11.5473     1.4651   7.881 4.07e-15 ***
    ## menarche             -3.5553     2.8946  -1.228 0.219407    
    ## momage                0.7629     1.2217   0.624 0.532390    
    ## mheight               9.7740    10.3099   0.948 0.343172    
    ## mraceBlack         -151.4348    46.0401  -3.289 0.001013 ** 
    ## mracePuerto Rican   -56.5081    45.1313  -1.252 0.210607    
    ## mraceAsian          -91.4092    71.9106  -1.271 0.203744    
    ## parity               95.5166    40.4743   2.360 0.018322 *  
    ## pnumlbw                   NA         NA      NA       NA    
    ## pnumsga                   NA         NA      NA       NA    
    ## ppbmi                 4.3385    14.8892   0.291 0.770769    
    ## ppwt                 -3.4707     2.6118  -1.329 0.183972    
    ## smoken               -4.8524     0.5868  -8.269  < 2e-16 ***
    ## wtgain                    NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 272.4 on 4321 degrees of freedom
    ## Multiple R-squared:  0.7183, Adjusted R-squared:  0.717 
    ## F-statistic:   551 on 20 and 4321 DF,  p-value: < 2.2e-16

Then we drop out those whose `p.value` is larger than 0.05 and refit the
model.

``` r
reg_lbt_m_sex = lm(bwt~ babysex + bhead + blength + delwt + gaweeks + parity + smoken, data = birthweight)
broom::tidy(reg_lbt_m_sex) %>% 
  select(term, estimate, p.value) %>% 
  knitr::kable()
```

| term          |      estimate |   p.value |
| :------------ | ------------: | --------: |
| (Intercept)   | \-6294.042648 | 0.0000000 |
| babysexfemale |     29.759772 | 0.0006604 |
| bhead         |    137.023526 | 0.0000000 |
| blength       |     78.868332 | 0.0000000 |
| delwt         |      2.074414 | 0.0000000 |
| gaweeks       |     14.357065 | 0.0000000 |
| parity        |    102.302116 | 0.0142252 |
| smoken        |    \-2.168761 | 0.0001965 |

The table shows that all the predictors now cnotribute to the fitted
model due to small `p.value`.

There for the model we set on the factors underying birthweight is
`birthweight ~ babysex + bhead + blength + delwt + gaweeks + parity +
smoken`.
