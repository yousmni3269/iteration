writing_functions
================
Soomin You
2024-10-24

Settings

Load key packages.

``` r
library(readxl)
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

## Writing my first function!!

as an example, here’s a z-score computation

``` r
x_vec = rnorm(n = 25, mean = 10, sd = 3.5)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.7710973 -0.2839624 -1.3712696  0.1892971  0.2426898  0.2140231
    ##  [7]  1.0271170  0.7839632  0.1825195  0.1144461  1.1254697 -0.7669447
    ## [13]  2.3168619  0.1457536  1.0960419 -2.3598060 -0.4553724  1.0220517
    ## [19] -1.3865298 -1.0739745  0.2784776 -0.3312083 -0.9561367 -0.3215256
    ## [25] -0.2030796

Now i’;; write a function to do this.

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)){
    stop("x needs to be numeric")
  }
  
  if (length(x) < 5) {
    stop("you need at least five numbers to compute the z score")
  }

  z = (x - mean(x)) / sd(x) 
  
  return(z)
  
}

z_scores(x = x_vec)
```

    ##  [1]  0.7710973 -0.2839624 -1.3712696  0.1892971  0.2426898  0.2140231
    ##  [7]  1.0271170  0.7839632  0.1825195  0.1144461  1.1254697 -0.7669447
    ## [13]  2.3168619  0.1457536  1.0960419 -2.3598060 -0.4553724  1.0220517
    ## [19] -1.3865298 -1.0739745  0.2784776 -0.3312083 -0.9561367 -0.3215256
    ## [25] -0.2030796

Does this always work?

``` r
z_scores(x = 3)
```

    ## Error in z_scores(x = 3): you need at least five numbers to compute the z score

``` r
z_scores(x = c("my", "name", "is", "jeff"))
```

    ## Error in z_scores(x = c("my", "name", "is", "jeff")): x needs to be numeric

## A new function!

``` r
mean_and_sd = function(x) {
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  out_df = 
    tibble(
      mean = mean_x,
      sd = sd_x 
    )
  
  return(out_df)
  
}

mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.8  4.04

## Check stuff using a simulation

``` r
sim_df = 
  tibble(
    x = rnorm(30, 10, 5)
  )

sim_df |>
  summarize(
    mean = mean(x), 
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.7  4.91

Simulation function to check sample mean and sd.

``` r
sim_mean_sd = function(samp_size, true_mean, true_sd){
  
  sim_df = 
    tibble(
      x = rnorm(samp_size, true_mean, true_sd)
  )
  
  out_df = 
    sim_df |>
    summarize(
      mean = mean(x), 
      sd = sd(x)
  )
  
  return(out_df)
  
}

sim_mean_sd(samp_size = 30, true_mean = 4, true_sd = 12)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.00  8.90

``` r
sim_mean_sd(true_mean = 4, true_sd = 12, samp_size = 30)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.72  11.4

``` r
sim_mean_sd(3000, 4, 12)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.04  12.1

## Revisit the LoTR words

``` r
fellowship_df = 
  read_excel("./data/LotR_Words.xlsx", range = "B3:D6") |>
  mutate(movie = "fellowship") |>
  janitor::clean_names()

two_towers_df = 
  read_excel("./data/LotR_Words.xlsx", range = "F3:H6") |>
  mutate(movie = "two_towers") |>
  janitor::clean_names()

return_king_df = 
  read_excel("./data/LotR_Words.xlsx", range = "J3:L6") |>
  mutate(movie = "return_king") |>
  janitor::clean_names()
```

Let’s do this using a function instead.

``` r
lotr_import = function(cell_range, movie_title) {
  
  movie_df = 
    read_excel("./data/LotR_Words.xlsx", range = cell_range) |>
    mutate(movie = movie_title) |>
    janitor::clean_names() |>
    pivot_longer(
      col = female:male, 
      names_to = "sex", 
      values_to = "words"
    ) |>
    select(movie, everything())
  
  return(movie_df)
  
}

lotr_import(cell_range = "B3:D6", movie_title = "fellowship")
```

    ## # A tibble: 6 × 4
    ##   movie      race   sex    words
    ##   <chr>      <chr>  <chr>  <dbl>
    ## 1 fellowship Elf    female  1229
    ## 2 fellowship Elf    male     971
    ## 3 fellowship Hobbit female    14
    ## 4 fellowship Hobbit male    3644
    ## 5 fellowship Man    female     0
    ## 6 fellowship Man    male    1995

``` r
lotr_import(cell_range = "F3:H6", movie_title = "two_towers")
```

    ## # A tibble: 6 × 4
    ##   movie      race   sex    words
    ##   <chr>      <chr>  <chr>  <dbl>
    ## 1 two_towers Elf    female   331
    ## 2 two_towers Elf    male     513
    ## 3 two_towers Hobbit female     0
    ## 4 two_towers Hobbit male    2463
    ## 5 two_towers Man    female   401
    ## 6 two_towers Man    male    3589

``` r
lotr_import("J3:L6", "return_king")
```

    ## # A tibble: 6 × 4
    ##   movie       race   sex    words
    ##   <chr>       <chr>  <chr>  <dbl>
    ## 1 return_king Elf    female   183
    ## 2 return_king Elf    male     510
    ## 3 return_king Hobbit female     2
    ## 4 return_king Hobbit male    2673
    ## 5 return_king Man    female   268
    ## 6 return_king Man    male    2459

``` r
lotr_df = 
  bind_rows(
    lotr_import(cell_range = "B3:D6", movie_title = "fellowship"), 
    lotr_import(cell_range = "F3:H6", movie_title = "two_towers"), 
    lotr_import("J3:L6", "return_king")
  )
```

## NSDUH

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)

marj_table = 
  nsduh_html |>
  html_table() |>
  nth(1) |>
  slice(-1) |>
  mutate(drug = "marj")

cocaine_table = 
  nsduh_html |>
  html_table() |>
  nth(4) |>
  slice(-1) |>
  mutate(drug = "cocaine")

heroin_table = 
  nsduh_html |>
  html_table() |>
  nth(5) |>
  slice(-1) |>
  mutate(drug = "heroin")
```

Make this into a function

``` r
nsduh_table_format = function(html, table_num, table_name){
  
  out_table = 
    html |>
    html_table() |>
    nth(table_num) |>
    slice(-1) |>
    mutate(drug = table_name) |>
    select(-contains("P Value"))
  
  return(out_table)
  
}

bind_rows(
  nsduh_table_format(html = nsduh_html, 1, "marj"), 
  nsduh_table_format(html = nsduh_html, 4, "cocaine"), 
  nsduh_table_format(html = nsduh_html, 5, "heroin")
)
```

    ## # A tibble: 168 × 12
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)` `12-17(2014-2015)`
    ##    <chr> <chr>            <chr>            <chr>              <chr>             
    ##  1 Tota… 12.90a           13.36            13.28b             12.86             
    ##  2 Nort… 13.88a           14.66            13.98              13.51             
    ##  3 Midw… 12.40b           12.76            12.45              12.33             
    ##  4 South 11.24a           11.64            12.02              11.88             
    ##  5 West  15.27            15.62            15.53a             14.43             
    ##  6 Alab… 9.98             9.60             9.90               9.71              
    ##  7 Alas… 19.60a           21.92            17.30              18.44             
    ##  8 Ariz… 13.69            13.12            15.12              13.45             
    ##  9 Arka… 11.37            11.59            12.79              12.14             
    ## 10 Cali… 14.49            15.25            15.03              14.11             
    ## # ℹ 158 more rows
    ## # ℹ 7 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>, drug <chr>

## DO THIS INSTEAD:

``` r
source("source/nsduh_table_format.R")

bind_rows(
  nsduh_table_format(html = nsduh_html, 1, "marj"), 
  nsduh_table_format(html = nsduh_html, 4, "cocaine"), 
  nsduh_table_format(html = nsduh_html, 5, "heroin")
)
```

    ## # A tibble: 168 × 12
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)` `12-17(2014-2015)`
    ##    <chr> <chr>            <chr>            <chr>              <chr>             
    ##  1 Tota… 12.90a           13.36            13.28b             12.86             
    ##  2 Nort… 13.88a           14.66            13.98              13.51             
    ##  3 Midw… 12.40b           12.76            12.45              12.33             
    ##  4 South 11.24a           11.64            12.02              11.88             
    ##  5 West  15.27            15.62            15.53a             14.43             
    ##  6 Alab… 9.98             9.60             9.90               9.71              
    ##  7 Alas… 19.60a           21.92            17.30              18.44             
    ##  8 Ariz… 13.69            13.12            15.12              13.45             
    ##  9 Arka… 11.37            11.59            12.79              12.14             
    ## 10 Cali… 14.49            15.25            15.03              14.11             
    ## # ℹ 158 more rows
    ## # ℹ 7 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>, drug <chr>
