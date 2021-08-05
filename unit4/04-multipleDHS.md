Combining multiple surveys and Post-stratification
================
Esteban Correa
7/30/2020

## Problem Statement

Let’s imagine you have a multipopulation survey such as the DHS (a
survey performed in several countries with a similar methodology or time
window). We want to perform a study across several countries. How we can
stack several countries without losing their sampling design? How to
combine complex surveys from different populations?

## DHS stacking

Well, in certain cases adding a country-sample-specific identifier to
the psu and strata variables would be enough to maintain the svydesign
calculations.

We can start by simulating two countries (Lesotho and South Africa) with
3 PSUs and 10 groups by strata each one.

``` r
set.seed(2019)


x <- data.frame( psu_variable = sample( 1:3 , 200 , replace = TRUE ) , strata_variable = sample( 1:10 , 200 , replace = TRUE ) , weight_variable = sample( 1:2 , 200 , replace = TRUE ) , some_variable = sample( 1:50 , 200 , replace = TRUE ) )

y <- data.frame( psu_variable = sample( 1:3 , 200 , replace = TRUE ) , strata_variable = sample( 1:10 , 200 , replace = TRUE ) , weight_variable = sample( 1:2 , 200 , replace = TRUE ) , some_variable = sample( 1:50 , 200 , replace = TRUE ) )
```

We can do the analysis for each separately:

``` r
# first country
# x
x[ , 'country_name' ] <- 'LSO'

# second country
# y
y[ , 'country_name' ] <- 'ZAF'


# possible design for first country
x_des <- svydesign( ~ psu_variable , strata = ~ strata_variable , data = x , weights = ~ weight_variable , nest = TRUE )

# possible design for second country
y_des <- svydesign( ~ psu_variable , strata = ~ strata_variable , data = y , weights = ~ weight_variable , nest = TRUE )
```

Or… We can create one full dataseta, creating new PSU and strata
variables:

``` r
# add a unique country identifier to psu and strata variables
x[ , 'new_psu_variable' ] <- paste( "LSO" , x[ , 'psu_variable' ] )
y[ , 'new_psu_variable' ] <- paste( "ZAF" , y[ , 'psu_variable' ] )
x[ , 'new_strata_variable' ] <- paste( "LSO" , x[ , 'strata_variable' ] )
y[ , 'new_strata_variable' ] <- paste( "ZAF" , y[ , 'strata_variable' ] )

# stack and create possible design
needed_variables <- c( 'new_psu_variable' , 'new_strata_variable' , 'weight_variable' , 'country_name' , 'some_variable' )
z <- rbind( x[ needed_variables ] , y[ needed_variables ] )

# possible stacked design
z_des <- svydesign( ~ new_psu_variable , strata = ~ new_strata_variable , data = z , weights = ~ weight_variable , nest = TRUE )
```

If everything was ok, mean and se should match in both modes:

``` r
# these statistics definitely need to line up
svymean( ~ some_variable , x_des )
```

    ##                 mean     SE
    ## some_variable 26.051 1.1394

``` r
svymean( ~ some_variable , y_des )
```

    ##                 mean     SE
    ## some_variable 27.189 0.8809

``` r
svyby( ~ some_variable , ~ country_name , z_des , svymean )
```

    ##     country_name some_variable        se
    ## LSO          LSO      26.05068 1.1394170
    ## ZAF          ZAF      27.18919 0.8808579

``` r
# combined result assumes x and y had appropriate weights
svymean( ~ some_variable , z_des )
```

    ##                mean     SE
    ## some_variable 26.62 0.7221

## Post-stratification

How can we make our analysis nationally representative? if we stacked
the datasets from the different nations, would the sum of weights match
their relative populations? As discussed by Gellman (See forum), in
order to make analysis nationally representative, a projection step
(post-stratification) is required using the real population of those
territories (LSO=1,047,087 and ZAF=28,049,364)

``` r
# do the weights match the country populations? Certainly not
sum( x[ , 'weight_variable' ] )
```

    ## [1] 296

``` r
sum( y[ , 'weight_variable' ] )
```

    ## [1] 296

``` r
# otherwise, post-stratify the design so it matches the relative population sizes
pop.types <- data.frame( country_name = c( 'LSO'  , 'ZAF' ) , Freq = c( 1047087 , 28049364 ) )
z_des_p <- postStratify( z_des , ~ country_name , pop.types )


# within-country estimates do not change
svyby( ~ some_variable , ~ country_name , z_des , svymean )
```

    ##     country_name some_variable        se
    ## LSO          LSO      26.05068 1.1394170
    ## ZAF          ZAF      27.18919 0.8808579

``` r
svyby( ~ some_variable , ~ country_name , z_des_p , svymean )
```

    ##     country_name some_variable        se
    ## LSO          LSO      26.05068 1.1394170
    ## ZAF          ZAF      27.18919 0.8808579

``` r
# combined country estimates now reflect relative population sizes for the `z_des_p` survey design
svymean( ~ some_variable , z_des )
```

    ##                mean     SE
    ## some_variable 26.62 0.7221

``` r
svymean( ~ some_variable , z_des_p )
```

    ##                 mean     SE
    ## some_variable 27.148 0.8501

# Real world example

Let’s do an stacking example for 27 DHS’ datasets

## References
