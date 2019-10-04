p8105\_hw2\_yj2580
================
yj2580
9/25/2019

## Problem 1

``` r
trash_df = read_excel("./data/HealthyHarborWaterWheelTotals2018-7-28.xlsx", sheet = 1)%>%
  janitor::clean_names() %>%
  select(-x15) %>%
  drop_na(dumpster)  %>%
  mutate(sports_balls = as.integer(round(as.numeric(sports_balls))))
```

    ## New names:
    ## * `` -> ...15

``` r
pre2018_df = read_excel("./data/HealthyHarborWaterWheelTotals2018-7-28.xlsx", sheet = 3, range = "A2:B14") %>%
  janitor::clean_names() %>%
  mutate(year = "2018")  %>%
  drop_na()

pre2017_df = read_excel("./data/HealthyHarborWaterWheelTotals2018-7-28.xlsx", sheet = 4, range = "A2:B14") %>%
  janitor::clean_names() %>%
  mutate(year = "2017")  %>%
  drop_na()

pre_df = left_join(pre2017_df, pre2018_df, by = "month") %>%
  mutate(month = month.name)
pre_df
```

    ## # A tibble: 12 x 5
    ##    month     total.x year.x total.y year.y
    ##    <chr>       <dbl> <chr>    <dbl> <chr> 
    ##  1 January      2.34 2017      0.96 2018  
    ##  2 February     1.46 2017      5.3  2018  
    ##  3 March        3.57 2017      2.18 2018  
    ##  4 April        3.99 2017      3.2  2018  
    ##  5 May          5.64 2017      9.27 2018  
    ##  6 June         1.4  2017      0.2  2018  
    ##  7 July         7.09 2017      2.39 2018  
    ##  8 August       4.44 2017     NA    <NA>  
    ##  9 September    1.95 2017     NA    <NA>  
    ## 10 October      0    2017     NA    <NA>  
    ## 11 November     0.11 2017     NA    <NA>  
    ## 12 December     0.94 2017     NA    <NA>

The total precipitation in 2018 was 0. The median number of sports balls
in a dumpster in 2017 was 8.

## Problem 2

``` r
pols_month_df = read_csv("./data/pols-month.csv") %>%
  janitor::clean_names()  %>%
  separate(mon, c("year", "month", "day")) %>%
  mutate(year = as.integer(year),
         month = as.integer(month),
         day = as.integer(day),
         month = as.factor(month.name[month]),
         president = ifelse(prez_gop == "1", "gop", "dem")) %>%
  select(-day, -prez_gop, -prez_dem)
```

    ## Parsed with column specification:
    ## cols(
    ##   mon = col_date(format = ""),
    ##   prez_gop = col_double(),
    ##   gov_gop = col_double(),
    ##   sen_gop = col_double(),
    ##   rep_gop = col_double(),
    ##   prez_dem = col_double(),
    ##   gov_dem = col_double(),
    ##   sen_dem = col_double(),
    ##   rep_dem = col_double()
    ## )

``` r
pols_month_df
```

    ## # A tibble: 822 x 9
    ##     year month    gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem president
    ##    <int> <fct>      <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr>    
    ##  1  1947 January       23      51     253      23      45     198 dem      
    ##  2  1947 February      23      51     253      23      45     198 dem      
    ##  3  1947 March         23      51     253      23      45     198 dem      
    ##  4  1947 April         23      51     253      23      45     198 dem      
    ##  5  1947 May           23      51     253      23      45     198 dem      
    ##  6  1947 June          23      51     253      23      45     198 dem      
    ##  7  1947 July          23      51     253      23      45     198 dem      
    ##  8  1947 August        23      51     253      23      45     198 dem      
    ##  9  1947 Septemb~      23      51     253      23      45     198 dem      
    ## 10  1947 October       23      51     253      23      45     198 dem      
    ## # ... with 812 more rows

``` r
snp_df = read_csv("./data/snp.csv") %>%
  janitor::clean_names()  %>%
  separate(date, c("month", "day", "year")) %>%
  mutate(year = as.integer(year),
         month = as.integer(month),
         day = as.integer(day),
         month = month.name[month],
         month = factor(month, levels = month.name)) %>%
  arrange(year, month) %>%
  select(year, month, everything())
```

    ## Parsed with column specification:
    ## cols(
    ##   date = col_character(),
    ##   close = col_double()
    ## )

``` r
snp_df
```

    ## # A tibble: 787 x 4
    ##     year month       day close
    ##    <int> <fct>     <int> <dbl>
    ##  1  1950 January       3  17.0
    ##  2  1950 February      1  17.2
    ##  3  1950 March         1  17.3
    ##  4  1950 April         3  18.0
    ##  5  1950 May           1  18.8
    ##  6  1950 June          1  17.7
    ##  7  1950 July          3  17.8
    ##  8  1950 August        1  18.4
    ##  9  1950 September     1  19.5
    ## 10  1950 October       2  19.5
    ## # ... with 777 more rows

``` r
unemployment_df = read_csv("./data/unemployment.csv") %>%
  janitor::clean_names()  %>%
  pivot_longer(
    jan:dec,
    names_to = "month", 
    values_to = "unemployment") %>%
  mutate(month = factor(month, labels = month.name),
         year = as.integer(year))
```

    ## Parsed with column specification:
    ## cols(
    ##   Year = col_double(),
    ##   Jan = col_double(),
    ##   Feb = col_double(),
    ##   Mar = col_double(),
    ##   Apr = col_double(),
    ##   May = col_double(),
    ##   Jun = col_double(),
    ##   Jul = col_double(),
    ##   Aug = col_double(),
    ##   Sep = col_double(),
    ##   Oct = col_double(),
    ##   Nov = col_double(),
    ##   Dec = col_double()
    ## )

``` r
unemployment_df
```

    ## # A tibble: 816 x 3
    ##     year month     unemployment
    ##    <int> <fct>            <dbl>
    ##  1  1948 May                3.4
    ##  2  1948 April              3.8
    ##  3  1948 August             4  
    ##  4  1948 January            3.9
    ##  5  1948 September          3.5
    ##  6  1948 July               3.6
    ##  7  1948 June               3.6
    ##  8  1948 February           3.9
    ##  9  1948 December           3.8
    ## 10  1948 November           3.7
    ## # ... with 806 more rows

``` r
#merge snp into pols
snp_pols = left_join(pols_month_df, snp_df, by = c("year", "month"))
```

    ## Warning: Column `month` joining factors with different levels, coercing to
    ## character vector

``` r
snp_pols
```

    ## # A tibble: 822 x 11
    ##     year month gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem president
    ##    <int> <chr>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr>    
    ##  1  1947 Janu~      23      51     253      23      45     198 dem      
    ##  2  1947 Febr~      23      51     253      23      45     198 dem      
    ##  3  1947 March      23      51     253      23      45     198 dem      
    ##  4  1947 April      23      51     253      23      45     198 dem      
    ##  5  1947 May        23      51     253      23      45     198 dem      
    ##  6  1947 June       23      51     253      23      45     198 dem      
    ##  7  1947 July       23      51     253      23      45     198 dem      
    ##  8  1947 Augu~      23      51     253      23      45     198 dem      
    ##  9  1947 Sept~      23      51     253      23      45     198 dem      
    ## 10  1947 Octo~      23      51     253      23      45     198 dem      
    ## # ... with 812 more rows, and 2 more variables: day <int>, close <dbl>

``` r
#merge unemployment into the result
snp_pols_unemploy = left_join(snp_pols, unemployment_df, by = c("year", "month"))
```

    ## Warning: Column `month` joining character vector and factor, coercing into
    ## character vector

``` r
snp_pols_unemploy
```

    ## # A tibble: 822 x 12
    ##     year month gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem president
    ##    <int> <chr>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr>    
    ##  1  1947 Janu~      23      51     253      23      45     198 dem      
    ##  2  1947 Febr~      23      51     253      23      45     198 dem      
    ##  3  1947 March      23      51     253      23      45     198 dem      
    ##  4  1947 April      23      51     253      23      45     198 dem      
    ##  5  1947 May        23      51     253      23      45     198 dem      
    ##  6  1947 June       23      51     253      23      45     198 dem      
    ##  7  1947 July       23      51     253      23      45     198 dem      
    ##  8  1947 Augu~      23      51     253      23      45     198 dem      
    ##  9  1947 Sept~      23      51     253      23      45     198 dem      
    ## 10  1947 Octo~      23      51     253      23      45     198 dem      
    ## # ... with 812 more rows, and 3 more variables: day <int>, close <dbl>,
    ## #   unemployment <dbl>

## Problem 3

``` r
pop_baby_name = read_csv("./data/Popular_Baby_Names.csv") %>%
  janitor::clean_names()  %>%
  mutate(childs_first_name = toupper(childs_first_name),
         ethnicity = ifelse(ethnicity == "ASIAN AND PACI", "ASIAN AND PACIFIC ISLANDER", ethnicity),
         ethnicity = ifelse(ethnicity == "BLACK NON HISP", "BLACK NON HISPANIC", ethnicity),
         ethnicity = ifelse(ethnicity == "WHITE NON HISP", "WHITE NON HISPANIC", ethnicity)) %>%
  distinct()
```

    ## Parsed with column specification:
    ## cols(
    ##   `Year of Birth` = col_double(),
    ##   Gender = col_character(),
    ##   Ethnicity = col_character(),
    ##   `Child's First Name` = col_character(),
    ##   Count = col_double(),
    ##   Rank = col_double()
    ## )

``` r
# the rank in popularity of the name “Olivia” as a female baby name over time
filter(pop_baby_name, childs_first_name == "OLIVIA") %>%
  select(year_of_birth, ethnicity, rank) %>%
  pivot_wider(
    names_from = "ethnicity",
    values_from = "rank"
  )
```

    ## # A tibble: 6 x 5
    ##   year_of_birth `ASIAN AND PACI~ `BLACK NON HISP~ HISPANIC `WHITE NON HISP~
    ##           <dbl>            <dbl>            <dbl>    <dbl>            <dbl>
    ## 1          2016                1                8       13                1
    ## 2          2015                1                4       16                1
    ## 3          2014                1                8       16                1
    ## 4          2013                3                6       22                1
    ## 5          2012                3                8       22                4
    ## 6          2011                4               10       18                2

``` r
# the most popular name among male children over time
filter(pop_baby_name, rank == "1", gender == "MALE") %>%
  select(year_of_birth, ethnicity, childs_first_name ) %>%
  pivot_wider(
    names_from = "ethnicity",
    values_from = "childs_first_name"
  )
```

    ## # A tibble: 6 x 5
    ##   year_of_birth `ASIAN AND PACI~ `BLACK NON HISP~ HISPANIC `WHITE NON HISP~
    ##           <dbl> <chr>            <chr>            <chr>    <chr>           
    ## 1          2016 ETHAN            NOAH             LIAM     JOSEPH          
    ## 2          2015 JAYDEN           NOAH             LIAM     DAVID           
    ## 3          2014 JAYDEN           ETHAN            LIAM     JOSEPH          
    ## 4          2013 JAYDEN           ETHAN            JAYDEN   DAVID           
    ## 5          2012 RYAN             JAYDEN           JAYDEN   JOSEPH          
    ## 6          2011 ETHAN            JAYDEN           JAYDEN   MICHAEL

``` r
male_whnohis = filter(pop_baby_name, gender == "MALE", ethnicity =="WHITE NON HISPANIC", year_of_birth == "2016")
scatterplot = ggplot(male_whnohis, aes(y = count, x = rank)) +geom_point()
scatterplot
```

![](hw2_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
