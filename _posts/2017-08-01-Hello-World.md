---
layout: post
---
### The markdown is here! 

This is the first test - probably the must have "Hello World" in markdown!

``` r
options(digits = 3)
cat("hello world!")
```

    ## hello world!

``` r
set.seed(123)
(x = rnorm(40) + 10)
```

    ##  [1]  9.44  9.77 11.56 10.07 10.13 11.72 10.46  8.73  9.31  9.55 11.22
    ## [12] 10.36 10.40 10.11  9.44 11.79 10.50  8.03 10.70  9.53  8.93  9.78
    ## [23]  8.97  9.27  9.37  8.31 10.84 10.15  8.86 11.25 10.43  9.70 10.90
    ## [34] 10.88 10.82 10.69 10.55  9.94  9.69  9.62

``` r
# generate a table
knitr::kable(head(mtcars))
```

|                   |   mpg|  cyl|  disp|   hp|  drat|    wt|  qsec|   vs|   am|  gear|  carb|
|-------------------|-----:|----:|-----:|----:|-----:|-----:|-----:|----:|----:|-----:|-----:|
| Mazda RX4         |  21.0|    6|   160|  110|  3.90|  2.62|  16.5|    0|    1|     4|     4|
| Mazda RX4 Wag     |  21.0|    6|   160|  110|  3.90|  2.88|  17.0|    0|    1|     4|     4|
| Datsun 710        |  22.8|    4|   108|   93|  3.85|  2.32|  18.6|    1|    1|     4|     1|
| Hornet 4 Drive    |  21.4|    6|   258|  110|  3.08|  3.21|  19.4|    1|    0|     3|     1|
| Hornet Sportabout |  18.7|    8|   360|  175|  3.15|  3.44|  17.0|    0|    0|     3|     2|
| Valiant           |  18.1|    6|   225|  105|  2.76|  3.46|  20.2|    1|    0|     3|     1|

``` r
f.print(5)
```

    ## [1] 5

``` r
plot(1:10)
```

![]({{ site.baseurl }}/img/2017-08-01-Hello-World/unnamed-chunk-2-1.png)
