cross validation
================
2022-11-28

## Step one

cross validation “by hand” on simulated data

``` r
nonlin_df = 
  tibble(
    id = 1:100,
    x = runif(100, 0, 1),
    y = 1 - 10 * (x - .3) ^ 2 + rnorm(100, 0, .3)
  )

nonlin_df %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point()
```

<img src="cross-validation_files/figure-gfm/unnamed-chunk-2-1.png" width="90%" />

Let’s get this by hand.

``` r
train_df = sample_n(nonlin_df,80)
test_df = anti_join(nonlin_df,train_df,by = "id") 
```

``` r
train_df %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point() +
  geom_point(data = test_df, color = "red")
```

<img src="cross-validation_files/figure-gfm/unnamed-chunk-4-1.png" width="90%" />

let’s fit three models.

``` r
linear_mod = lm(y~x, data = train_df)
smooth_mod = mgcv::gam(y~s(x),data = train_df)
wiggly_mod = mgcv::gam(y~s(x, k = 30), sp = 10e-6, data = train_df)
```

let’s see the results.

``` r
train_df %>% 
  add_predictions(wiggly_mod) %>% 
  ggplot(aes(x=x, y=y))+
  geom_point()+
  geom_line(aes(y= pred), color = "red")
```

<img src="cross-validation_files/figure-gfm/unnamed-chunk-6-1.png" width="90%" />

let’s make predictions and compute RMSEs.

``` r
rmse(linear_mod,test_df)
```

    ## [1] 0.7776684

``` r
rmse(smooth_mod,test_df)
```

    ## [1] 0.3558412

``` r
rmse(wiggly_mod,test_df)
```

    ## [1] 0.4568811

## can we iterate…?

``` r
cv_df = 
  crossv_mc(nonlin_df,100) %>% 
  mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble)
  ) %>% 
  mutate(
    linear_fits = map(.x = train, ~lm(y~x, data = .x)),
    smooth_fits = map(.x = train, ~gam(y~s(x), data = .x)),
    wiggly_fits = map(.x = train, ~gam(y~s(x, k =30), sp = 10e-6, data = .x))
  ) %>% 
   mutate(
     rmse_linear = map2_dbl(.x = linear_fits, .y = test, ~rmse(model = .x, data = .y)),
     rmse_smooth = map2_dbl(.x = smooth_fits, .y = test, ~rmse(model = .x, data = .y)), 
     rmse_wiggly = map2_dbl(.x = wiggly_fits, .y = test, ~rmse(model = .x, data = .y))
  )
```

``` r
cv_df %>% pull(train) %>% .[[1]] %>% as_tibble
```

    ## # A tibble: 79 × 3
    ##       id     x       y
    ##    <int> <dbl>   <dbl>
    ##  1     3 0.645 -0.474 
    ##  2     4 0.154  0.421 
    ##  3     5 0.473  1.04  
    ##  4     6 0.570  0.197 
    ##  5     7 0.625 -0.352 
    ##  6     9 0.489  1.03  
    ##  7    11 0.229  1.10  
    ##  8    12 0.490  0.0918
    ##  9    13 0.597 -0.0794
    ## 10    14 0.232  1.10  
    ## # … with 69 more rows

what do these results say about the model choices?

``` r
cv_df %>% 
  select(starts_with("rmse")) %>% 
  pivot_longer(
    everything(),
    names_to = "model",
    values_to = "rmse",
    names_prefix = "rmse_ "
  ) %>% 
  ggplot(aes(x= model,y =rmse))+
  geom_violin()
```

<img src="cross-validation_files/figure-gfm/unnamed-chunk-10-1.png" width="90%" />

compute average..

``` r
cv_df %>% 
  select(starts_with("rmse")) %>% 
  pivot_longer(
    everything(),
    names_to = "model",
    values_to = "rmse",
    names_prefix = "rmse_ "
  ) %>% 
  group_by(model) %>% 
  summarize(ave_rmse =mean(rmse))
```

    ## # A tibble: 3 × 2
    ##   model       ave_rmse
    ##   <chr>          <dbl>
    ## 1 rmse_linear    0.819
    ## 2 rmse_smooth    0.326
    ## 3 rmse_wiggly    0.391

## try on a real dataset

``` r
growth_df = read_csv("./data/nepalese_children.csv")
```

    ## Rows: 2705 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (5): age, sex, weight, height, armc
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
growth_df %>% 
  ggplot(aes(x = weight, y = armc))+
  geom_point(alpha=.3) 
```

<img src="cross-validation_files/figure-gfm/unnamed-chunk-13-1.png" width="90%" />

Brief aside on piecewise linear models.

``` r
growth_df = 
  growth_df %>% 
  mutate(
    weight_pwl =(weight>7)*(weight-7)
  )
```

``` r
pwl_model = lm(armc~weight+weight_pwl, data = growth_df)
linear_model = lm(armc~weight, data = growth_df)
smooth_model = mgcv::gam(armc~s(weight), data = growth_df)
```

``` r
growth_df %>% 
  add_predictions(smooth_model) %>% 
  ggplot(aes(x=weight, y = armc))+
  geom_point(alpha =.3)+
  geom_line(aes(y = pred), color = "red")
```

<img src="cross-validation_files/figure-gfm/unnamed-chunk-16-1.png" width="90%" />

``` r
cv_df_2 = 
  crossv_mc(growth_df,100) %>% 
  mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble)
  )


cv_df_2 = 
cv_df_2 %>% 
    mutate(
    pwl_fits = map(.x = train, ~lm(armc~weight+weight_pwl, data = .x)),
    linear_fits = map(.x = train, ~lm(armc~weight, data = .x)),
    smooth_fits = map(.x = train, ~gam(armc~s(weight), data = .x))
  ) %>% 
   mutate(
     rmse_pwl = map2_dbl(.x = pwl_fits, .y = test, ~rmse(model = .x, data = .y)),
     rmse_linear = map2_dbl(.x = linear_fits, .y = test, ~rmse(model = .x, data = .y)), 
     rmse_smooth = map2_dbl(.x = smooth_fits, .y = test, ~rmse(model = .x, data = .y))
  )
```

let’s look at the results.

``` r
cv_df_2 %>% 
  select(starts_with("rmse")) %>% 
  pivot_longer(
    everything(),
    names_to = "model",
    values_to = "rmse",
    names_prefix = "rmse_ "
  ) %>% 
  ggplot(aes(x= model,y =rmse))+
  geom_boxplot()
```

<img src="cross-validation_files/figure-gfm/unnamed-chunk-18-1.png" width="90%" />
