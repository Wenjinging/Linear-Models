linear models
================
2022-11-12

Load NYC Airbnb dataset

``` r
data("nyc_airbnb")

nyc_airbnb = 
  nyc_airbnb %>% 
  mutate(stars = review_scores_location / 2) %>% 
  rename(
    borough = neighbourhood_group,
    neighborhood = neighbourhood) %>% 
  filter(borough != "Staten Island") %>% 
  select(price, stars, borough, neighborhood, room_type)
```

## Fit the first model

``` r
fit = lm(price~stars + borough, data = nyc_airbnb)

fit
```

    ## 
    ## Call:
    ## lm(formula = price ~ stars + borough, data = nyc_airbnb)
    ## 
    ## Coefficients:
    ##      (Intercept)             stars   boroughBrooklyn  boroughManhattan  
    ##           -70.41             31.99             40.50             90.25  
    ##    boroughQueens  
    ##            13.21

``` r
summary(fit)
```

    ## 
    ## Call:
    ## lm(formula = price ~ stars + borough, data = nyc_airbnb)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -169.8  -64.0  -29.0   20.2 9870.0 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       -70.414     14.021  -5.022 5.14e-07 ***
    ## stars              31.990      2.527  12.657  < 2e-16 ***
    ## boroughBrooklyn    40.500      8.559   4.732 2.23e-06 ***
    ## boroughManhattan   90.254      8.567  10.534  < 2e-16 ***
    ## boroughQueens      13.206      9.065   1.457    0.145    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 181.5 on 30525 degrees of freedom
    ##   (9962 observations deleted due to missingness)
    ## Multiple R-squared:  0.03423,    Adjusted R-squared:  0.03411 
    ## F-statistic: 270.5 on 4 and 30525 DF,  p-value: < 2.2e-16

``` r
fit %>%
  broom::tidy()%>%
  mutate(
    term = str_replace(term, "borough", "Borough:")
  ) %>%
  select(term, estimate, p.value) %>%
  knitr::kable(digits = 2)
```

| term              | estimate | p.value |
|:------------------|---------:|--------:|
| (Intercept)       |   -70.41 |    0.00 |
| stars             |    31.99 |    0.00 |
| Borough:Brooklyn  |    40.50 |    0.00 |
| Borough:Manhattan |    90.25 |    0.00 |
| Borough:Queens    |    13.21 |    0.15 |

Let’s change reference category

``` r
fit = 
  nyc_airbnb %>%
  mutate(
    borough = fct_infreq(borough)
  ) %>%
  lm(price~stars + borough, data = . )


fit %>%
  broom::tidy()%>%
  mutate(
    term = str_replace(term, "borough", "Borough:")
  ) %>%
  select(term, estimate, p.value) %>%
  knitr::kable(digits = 2) 
```

| term             | estimate | p.value |
|:-----------------|---------:|--------:|
| (Intercept)      |    19.84 |     0.1 |
| stars            |    31.99 |     0.0 |
| Borough:Brooklyn |   -49.75 |     0.0 |
| Borough:Queens   |   -77.05 |     0.0 |
| Borough:Bronx    |   -90.25 |     0.0 |

``` r
fit %>%
  broom::glance() %>%
  select(AIC)
```

    ## # A tibble: 1 × 1
    ##       AIC
    ##     <dbl>
    ## 1 404237.
