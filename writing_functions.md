writing_functions
================
Soomin You
2024-10-24

Settings

Load key packages.

``` r
library(tidyverse)
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

    ##  [1]  2.6638486 -0.8674058 -1.5811070 -0.3613824 -1.1237306 -0.9842967
    ##  [7]  0.4967776  0.4693495  0.6915750  1.7608497 -0.4145652 -0.1460518
    ## [13] -0.3217979  0.6369307  0.8976203 -0.6381714 -0.3731413  0.6718184
    ## [19] -0.1141708 -0.6786921  0.6039478 -1.7118678 -0.6371157  0.5344022
    ## [25]  0.5263766

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

    ##  [1]  2.6638486 -0.8674058 -1.5811070 -0.3613824 -1.1237306 -0.9842967
    ##  [7]  0.4967776  0.4693495  0.6915750  1.7608497 -0.4145652 -0.1460518
    ## [13] -0.3217979  0.6369307  0.8976203 -0.6381714 -0.3731413  0.6718184
    ## [19] -0.1141708 -0.6786921  0.6039478 -1.7118678 -0.6371157  0.5344022
    ## [25]  0.5263766

Does this always work?

``` r
z_scores(x = 3)
```

    ## Error in z_scores(x = 3): you need at least five numbers to compute the z score

``` r
z_scores(x = c("my", "name", "is", "jeff"))
```

    ## Error in z_scores(x = c("my", "name", "is", "jeff")): x needs to be numeric
