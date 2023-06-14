---
title: "Homework 1"
author: "Seán O'Doherty"
date: 2023-05-14
format: 
  docx: default
  html:
    toc: true
    toc_float: true
    code-fold: true
editor: visual
---




# Data Manipulation

## Problem 1: Use logical operators to find flights that:

```         
-   Had an arrival delay of two or more hours (\> 120 minutes)
-   Flew to Houston (IAH or HOU)
-   Were operated by United (`UA`), American (`AA`), or Delta (`DL`)
-   Departed in summer (July, August, and September)
-   Arrived more than two hours late, but didn't leave late
-   Were delayed by at least an hour, but made up over 30 minutes in flight
```



```r
# Had an arrival delay of two or more hours (> 120 minutes)
flights %>%
  filter(arr_delay > 120)
```

```
## # A tibble: 10,034 × 19
##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
##  1  2013     1     1      811            630       101     1047            830
##  2  2013     1     1      848           1835       853     1001           1950
##  3  2013     1     1      957            733       144     1056            853
##  4  2013     1     1     1114            900       134     1447           1222
##  5  2013     1     1     1505           1310       115     1638           1431
##  6  2013     1     1     1525           1340       105     1831           1626
##  7  2013     1     1     1549           1445        64     1912           1656
##  8  2013     1     1     1558           1359       119     1718           1515
##  9  2013     1     1     1732           1630        62     2028           1825
## 10  2013     1     1     1803           1620       103     2008           1750
## # ℹ 10,024 more rows
## # ℹ 11 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
## #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
## #   hour <dbl>, minute <dbl>, time_hour <dttm>
```

```r
# Flew to Houston (IAH or HOU)
flights %>% 
  filter(dest %in% c("IAH", "Hou"))
```

```
## # A tibble: 7,198 × 19
##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
##  1  2013     1     1      517            515         2      830            819
##  2  2013     1     1      533            529         4      850            830
##  3  2013     1     1      623            627        -4      933            932
##  4  2013     1     1      728            732        -4     1041           1038
##  5  2013     1     1      739            739         0     1104           1038
##  6  2013     1     1      908            908         0     1228           1219
##  7  2013     1     1     1028           1026         2     1350           1339
##  8  2013     1     1     1044           1045        -1     1352           1351
##  9  2013     1     1     1114            900       134     1447           1222
## 10  2013     1     1     1205           1200         5     1503           1505
## # ℹ 7,188 more rows
## # ℹ 11 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
## #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
## #   hour <dbl>, minute <dbl>, time_hour <dttm>
```

```r
# Were operated by United (`UA`), American (`AA`), or Delta (`DL`)
flights %>% 
  filter(carrier %in% c("UA", "AA", "DL"))
```

```
## # A tibble: 139,504 × 19
##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
##  1  2013     1     1      517            515         2      830            819
##  2  2013     1     1      533            529         4      850            830
##  3  2013     1     1      542            540         2      923            850
##  4  2013     1     1      554            600        -6      812            837
##  5  2013     1     1      554            558        -4      740            728
##  6  2013     1     1      558            600        -2      753            745
##  7  2013     1     1      558            600        -2      924            917
##  8  2013     1     1      558            600        -2      923            937
##  9  2013     1     1      559            600        -1      941            910
## 10  2013     1     1      559            600        -1      854            902
## # ℹ 139,494 more rows
## # ℹ 11 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
## #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
## #   hour <dbl>, minute <dbl>, time_hour <dttm>
```

```r
# Departed in summer (July, August, and September)
flights %>% 
  filter(month %in% c(7, 8, 9))
```

```
## # A tibble: 86,326 × 19
##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
##  1  2013     7     1        1           2029       212      236           2359
##  2  2013     7     1        2           2359         3      344            344
##  3  2013     7     1       29           2245       104      151              1
##  4  2013     7     1       43           2130       193      322             14
##  5  2013     7     1       44           2150       174      300            100
##  6  2013     7     1       46           2051       235      304           2358
##  7  2013     7     1       48           2001       287      308           2305
##  8  2013     7     1       58           2155       183      335             43
##  9  2013     7     1      100           2146       194      327             30
## 10  2013     7     1      100           2245       135      337            135
## # ℹ 86,316 more rows
## # ℹ 11 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
## #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
## #   hour <dbl>, minute <dbl>, time_hour <dttm>
```

```r
# Arrived more than two hours late, but didn't leave late
flights %>% 
  filter(arr_delay > 120, dep_delay <= 0)
```

```
## # A tibble: 29 × 19
##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
##  1  2013     1    27     1419           1420        -1     1754           1550
##  2  2013    10     7     1350           1350         0     1736           1526
##  3  2013    10     7     1357           1359        -2     1858           1654
##  4  2013    10    16      657            700        -3     1258           1056
##  5  2013    11     1      658            700        -2     1329           1015
##  6  2013     3    18     1844           1847        -3       39           2219
##  7  2013     4    17     1635           1640        -5     2049           1845
##  8  2013     4    18      558            600        -2     1149            850
##  9  2013     4    18      655            700        -5     1213            950
## 10  2013     5    22     1827           1830        -3     2217           2010
## # ℹ 19 more rows
## # ℹ 11 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
## #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
## #   hour <dbl>, minute <dbl>, time_hour <dttm>
```

```r
# Were delayed by at least an hour, but made up over 30 minutes in flight
flights %>% 
  filter(dep_delay >= 60, dep_delay - arr_delay > 30)
```

```
## # A tibble: 1,844 × 19
##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
##  1  2013     1     1     2205           1720       285       46           2040
##  2  2013     1     1     2326           2130       116      131             18
##  3  2013     1     3     1503           1221       162     1803           1555
##  4  2013     1     3     1839           1700        99     2056           1950
##  5  2013     1     3     1850           1745        65     2148           2120
##  6  2013     1     3     1941           1759       102     2246           2139
##  7  2013     1     3     1950           1845        65     2228           2227
##  8  2013     1     3     2015           1915        60     2135           2111
##  9  2013     1     3     2257           2000       177       45           2224
## 10  2013     1     4     1917           1700       137     2135           1950
## # ℹ 1,834 more rows
## # ℹ 11 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
## #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
## #   hour <dbl>, minute <dbl>, time_hour <dttm>
```


## Problem 2: What months had the highest and lowest proportion of cancelled flights? Interpret any seasonal patterns. To determine if a flight was cancelled use the following code

<!-- -->

```         
flights %>% 
  filter(is.na(dep_time)) 
```



```r
# // What months had the highest and lowest % of cancelled flights?

# Assign to new table
flights_month_prop_cancelled <-
  
  flights %>%
    
    # Group by month
    group_by(month) %>% 
    
    # calculate the sum of non-na flights divided by count of flights
    summarize("prop_cancelled" = sum(is.na(dep_time)) / n())


# Print results sorted by prop_cancelled
flights_month_prop_cancelled %>% 
   
  # sort by prop_cancelled to get low to high
  arrange(prop_cancelled)
```

```
## # A tibble: 12 × 2
##    month prop_cancelled
##    <int>          <dbl>
##  1    10        0.00817
##  2    11        0.00854
##  3     9        0.0164 
##  4     8        0.0166 
##  5     1        0.0193 
##  6     5        0.0196 
##  7     4        0.0236 
##  8     3        0.0299 
##  9     7        0.0319 
## 10     6        0.0357 
## 11    12        0.0364 
## 12     2        0.0505
```

```r
# Plot line graph
ggplot(
  
  # use dataset from above
  flights_month_prop_cancelled
  
  # set x and y axes, use factor(to make month discreet)
  ,aes(
    x = factor(month)
    ,y = percent(prop_cancelled, 1)
    ,group=1
  )

) +
  
  # add line to graph
  geom_col(
    fill = "blue"
  ) +
  
  # Add value labels
  geom_text(
    aes(
      label = percent(prop_cancelled, 0.01)
    )
    ,color = "white"
    ,hjust = 0.5
    ,vjust = 2
    ,size = 2.5
  ) +
  
  # Add Axis labels
  labs(
    title = "The lowest proportion of cancelled flights is in October \nand the highest is in February"
    ,x = "Month"
    ,y = "Proportion of flights cancelled"
  ) +
  
  # Apply theme
  theme_light()
```

<img src="/english/post/homework1_seanodoherty_files/figure-html/problem-2-1.png" width="672" />


The lowest proportion of cancelled flights is in October and the highest is in February.

## Problem 3: What plane (specified by the `tailnum` variable) traveled the most times from New York City airports in 2013? Please `left_join()` the resulting table with the table `planes` (also included in the `nycflights13` package).

For the plane with the greatest number of flights and that had more than 50 seats, please create a table where it flew to during 2013.



```r
# store to new table
flights_planes_50p <-
  
  # Left join flights to planes to get the number of seats
  left_join(x = flights, y = planes, by = "tailnum") %>% 
  
    # Filter to only planes with > 50 seats / flights which have a dep_time
    filter(seats > 50, !is.na(dep_time))


# store to new table
planes_50p_flight_count_top1 <-
  
  # use top_n to return only the top row 
  top_n(
    flights_planes_50p %>%
      
      # Group by plane and count flights
      group_by(tailnum) %>%
      
      # Sort descending
      summarise(flight_count = n()) %>%
      arrange(desc(flight_count))
    
    ,1
    )
```

```
## Selecting by flight_count
```

```r
# Display results
planes_50p_flight_count_top1
```

```
## # A tibble: 1 × 2
##   tailnum flight_count
##   <chr>          <int>
## 1 N328AA           389
```

```r
# store to new table
flights_planes_50p_flight_count_top1 <-

  # join the flights of planes with 50+ seats to table with top 1 by flight count 
  right_join(
    flights_planes_50p
    ,planes_50p_flight_count_top1
    ,by = "tailnum"
  ) %>% 
    
    # Group by destination
    group_by(dest) %>%
      
    #summarise to get count of flights (with dep_time)
    summarise(dest_flight_count = sum(!is.na(dep_time)))
  
# print table
flights_planes_50p_flight_count_top1
```

```
## # A tibble: 6 × 2
##   dest  dest_flight_count
##   <chr>             <int>
## 1 BOS                   1
## 2 LAX                 310
## 3 MCO                   1
## 4 MIA                  25
## 5 SFO                  51
## 6 SJU                   1
```

```r
# Create ggplot
ggplot(
  
  # call previous table
  flights_planes_50p_flight_count_top1
  
  # define plot aesthetics
  ,aes(
    x = dest
    ,y = dest_flight_count
    ,fill = dest
  )

) +
  
  # Add bar to chart
  geom_bar(stat = "identity") +
  
  # Add value labels
  geom_text(aes(label = dest_flight_count), vjust = -0.2) +
  
  # Add title and axis labels
  labs(
    x = "Destination Airport Code"
    ,y = "Flights to Destination"
    ,title = "The plane (50+ seats) which flew most times from NYC in 2013 \nmostly flew to LAX"
  ) +
  
  # Apply theme
  theme_light()
```

<img src="/english/post/homework1_seanodoherty_files/figure-html/problem-3-1.png" width="672" />


The plane (which has 50 or more seats) which flew the most times from NYC is tailnum N328AA.

The breakdown of where this plane flew to is included in table and bar chart form. There were many flights to LAX and some to SFO and MIA. There was only 1 flight to each of BOS, MCO and SJU.

## Problem 4: The `nycflights13` package includes a table (`weather`) that describes the weather during 2013. Use that table to answer the following questions:

```         
-   What is the distribution of temperature (`temp`) in July 2013? Identify any important outliers in terms of the `wind_speed` variable.
-   What is the relationship between `dewp` and `humid`?
-   What is the relationship between `precip` and `visib`?
```



```r
# // What is the distribution of temperature (`temp`) in July 2013? Identify any important outliers in terms of the `wind_speed` variable.

# define function to find outliers
findoutlier <-
  function(x) {
    return(x < quantile(x, .25) - 1.5*IQR(x) | x > quantile(x, .75) + 1.5*IQR(x))
}

# store to new table
weather_july <-
  
  # filter weather to july
  filter(weather, month == 7)

# histogram of temperature
ggplot(
  weather_july
  ,aes(x = temp)
) +
  geom_histogram() +
  theme_light()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

<img src="/english/post/homework1_seanodoherty_files/figure-html/problem-4-1.png" width="672" />

```r
# store to table
weather_july_windspeed <-

  # filter out na values of windspeed
  weather_july %>%
    filter(!is.na(wind_speed)) %>% 
    
    # Create new col which identifies wind_speed outliers
    mutate(
       wind_speed_outlier =
         ifelse(
           findoutlier(wind_speed)
           ,"Outlier (wind_speed)"
           ,"Not Outlier (wind_speed)"
          )
       )

# Create plot of wind speed through time with color identifying outliers
ggplot(
  weather_july_windspeed
  ,aes(
    x = time_hour
    ,y = wind_speed
    ,color = wind_speed_outlier
  )
) +
  
  # Add scatter
  geom_point() +
  
  # Add labels and theme
  labs(
    x = "Time"
    ,y = "Windspeed"
    ,title = "July 2013 Windpseed had a few outliers"
    ,colour = "Windspeed Outlier"
  ) +
  theme_light()
```

<img src="/english/post/homework1_seanodoherty_files/figure-html/problem-4-2.png" width="672" />

```r
# // What is the relationship between `dewp` and `humid`?
  
# Create plot of dewp vs humid
ggplot(
  weather
  ,aes(
    x = dewp
    ,y = humid
    )
) +
  
  # make scatter plot
  geom_point() +
  
  # Use stat_smooth to add a regression line
  stat_smooth(
    method = "lm",
    formula = y ~ x,
    geom = "smooth"
  ) +
  
  # Add labels and theme
  labs(
    x = "Dewpoint Temperature"
    ,y = "Humidity"
    ,title = "Dewpoint Temperature and Humidity are positively correlated"
  ) +
  theme_light()
```

```
## Warning: Removed 1 rows containing non-finite values (`stat_smooth()`).
```

```
## Warning: Removed 1 rows containing missing values (`geom_point()`).
```

<img src="/english/post/homework1_seanodoherty_files/figure-html/problem-4-3.png" width="672" />

```r
# // What is the relationship between `precip` and `visib`?

# Scatter plot of precip vs visib
# Use stat_smooth to add a regression line
# Convert x axis to log scale to make easier to read

# Create plot of precip vs visib
ggplot(
  weather
  ,aes(x = precip, y = visib)
) +
  
  # add scatter
  geom_point() +
  
  # add regression line
  stat_smooth(
    method = "lm",
    formula = y ~ x,
    geom = "smooth"
  ) +
  
  # put x on a log scale to aid leibility
  scale_x_log10(
  ) +
  
  # add labels and theme
  labs(
    x = "Precipitation"
    ,y = "Visibility"
    ,title = "Precipitation and Visibility are negatively correlated"
  ) +
  theme_light()
```

```
## Warning: Transformation introduced infinite values in continuous x-axis
```

```
## Warning: Transformation introduced infinite values in continuous x-axis
```

```
## Warning: Removed 24366 rows containing non-finite values (`stat_smooth()`).
```

<img src="/english/post/homework1_seanodoherty_files/figure-html/problem-4-4.png" width="672" />


## Problem 5: Use the `flights` and `planes` tables to answer the following questions:

```         
-   How many planes have a missing date of manufacture?
-   What are the five most common manufacturers?
-   Has the distribution of manufacturer changed over time as reflected by the airplanes flying from NYC in 2013? (Hint: you may need to use case_when() to recode the manufacturer name and collapse rare vendors into a category called Other.)
```



```r
# // How many planes have a missing date of manufacture?

planes %>%
  
  # filter to planes which do not have a manufacture year
  filter(is.na(year)) %>% 
  
  # return count
  summarise(planecount = n())
```

```
## # A tibble: 1 × 1
##   planecount
##        <int>
## 1         70
```

```r
# // What are the five most common manufacturers?

# store to new table
manuf_flightcount_planecount <-
  
  # join flights which have dep_time to planes by tailnum
  inner_join(
    x = filter(flights, !is.na(dep_time))
    ,y = planes
    ,by = "tailnum"
  ) %>%
  
    # group by manufacturer
    group_by(manufacturer) %>% 
    
    # use summarise to get plane_count and flight count
    summarise(plane_count = n_distinct(tailnum), flight_count = n())

# store results top 5 by plane_count and flight_count
manuf_planecount_top5 <-
  top_n(
    manuf_flightcount_planecount %>% 
        arrange(desc(plane_count))
      ,5
  )
```

```
## Selecting by flight_count
```

```r
# store results top 5 by plane_count and flight_count
manuf_flightcount_top5 <-
  top_n(
    manuf_flightcount_planecount %>% 
        arrange(desc(flight_count))
      ,5
  )
```

```
## Selecting by flight_count
```

```r
#display results
manuf_planecount_top5
```

```
## # A tibble: 5 × 3
##   manufacturer     plane_count flight_count
##   <chr>                  <int>        <int>
## 1 BOEING                  1627        82524
## 2 AIRBUS INDUSTRIE         400        40753
## 3 BOMBARDIER INC           366        27588
## 4 AIRBUS                   336        47009
## 5 EMBRAER                  299        63783
```

```r
manuf_flightcount_top5
```

```
## # A tibble: 5 × 3
##   manufacturer     plane_count flight_count
##   <chr>                  <int>        <int>
## 1 BOEING                  1627        82524
## 2 EMBRAER                  299        63783
## 3 AIRBUS                   336        47009
## 4 AIRBUS INDUSTRIE         400        40753
## 5 BOMBARDIER INC           366        27588
```

```r
# // Has the distribution of manufacturer changed over time as reflected by the airplanes flying from NYC in 2013? (Hint: you may need to use case_when() to recode the manufacturer name and collapse rare vendors into a category called Other.)

# Left join flights (with dep_time) to planes on tailnumber to get manufacturer
# Left join to manuf_planecount_top5 to identify top 5 (use keep = TRUE)
# Mutate NAs to "OTHER"
# Group by Manufacturer and month and count flights

# store to new table
flights_manuf_month <-
  
  # Join the flights with manufacturer to the top 5 manufacturers by flight count
  left_join(
    x =
      
      # join  flights which have dep_time to planes by tailnum to get manufacturer
      left_join(
        x = filter(flights, !is.na(dep_time))
        ,y = planes
        ,by = "tailnum"
      )
      ,y = manuf_flightcount_top5
      ,by = "manufacturer"
      ,keep = TRUE
  ) %>%
    
    # Replace the manufacturer col from the top5 table with "OTHER" where there is no match
    mutate(manufacturer = replace_na(manufacturer.y, "OTHER"))  %>%
    
    # group by manufacturer (and other) and month
    group_by(manufacturer, month) %>%
    
    # get flight counts
    summarise(flight_count = n())
```

```
## `summarise()` has grouped output by 'manufacturer'. You can override using the
## `.groups` argument.
```

```r
# Create plot of flight count by month with manufacturers in fill
ggplot(
  flights_manuf_month
  ,aes(x = month, y = flight_count, fill = manufacturer)
) +
  
  # Add stacked bars
  geom_bar(position = "stack", stat = "identity"
) +
  
  # Add labels and theme
  labs(
    x = "Month"
    ,y = "Flight Count"
    ,fill = "Manufacturer"
    ,title = "Flights by Manufacturer by Month"
  ) +
  theme_light()
```

<img src="/english/post/homework1_seanodoherty_files/figure-html/problem-5-1.png" width="672" />

```r
# Create plot of flight count by month with manufacturers in fill
ggplot(
  flights_manuf_month
  ,aes(x = month, y = flight_count, fill = manufacturer)
) +
  
  # add pct of total bars
  geom_bar(position = "fill", stat = "identity"
) +
  
  # Add labels and theme
  labs(
    x = "Month"
    ,y = "Pct of Flights"
    ,fill = "Manufacturer"
    ,title = "Pct of Flights by Manufacturer by Month"
  ) +
  theme_light()
```

<img src="/english/post/homework1_seanodoherty_files/figure-html/problem-5-2.png" width="672" />


## Problem 6: Use the `flights` and `planes` tables to answer the following questions:

```         
-   What is the oldest plane (specified by the tailnum variable) that flew from New York City airports in 2013?
-   How many airplanes that flew from New York City are included in the planes table?
```



```r
# // What is the oldest plane (specified by the tailnum variable) that flew from New York City airports in 2013?

# join tailnums with dep_time to planes to get manufacture year
left_join(
  
  # lsit of tailnums with dep_time
  x = flights %>% 
    filter(!is.na(dep_time)) %>% 
    group_by(tailnum) %>% 
    summarise()
 ,y = planes
 ,by = "tailnum"
) %>%
  
  # sort by year ascending
  arrange(year)
```

```
## # A tibble: 4,037 × 9
##    tailnum  year type              manufacturer model engines seats speed engine
##    <chr>   <int> <chr>             <chr>        <chr>   <int> <int> <int> <chr> 
##  1 N381AA   1956 Fixed wing multi… DOUGLAS      DC-7…       4   102   232 Recip…
##  2 N201AA   1959 Fixed wing singl… CESSNA       150         1     2    90 Recip…
##  3 N567AA   1959 Fixed wing singl… DEHAVILLAND  OTTE…       1    16    95 Recip…
##  4 N378AA   1963 Fixed wing singl… CESSNA       172E        1     4   105 Recip…
##  5 N575AA   1963 Fixed wing singl… CESSNA       210-…       1     6    NA Recip…
##  6 N14629   1965 Fixed wing multi… BOEING       737-…       2   149    NA Turbo…
##  7 N615AA   1967 Fixed wing multi… BEECH        65-A…       2     9   202 Turbo…
##  8 N425AA   1968 Fixed wing singl… PIPER        PA-2…       1     4   107 Recip…
##  9 N383AA   1972 Fixed wing multi… BEECH        E-90        2    10    NA Turbo…
## 10 N364AA   1973 Fixed wing multi… CESSNA       310Q        2     6   167 Recip…
## # ℹ 4,027 more rows
```

```r
# // How many airplanes that flew from New York City are included in the planes table?

#Inner join flights to planes to get matches
inner_join(
  
  x = flights %>%
        
    # filter to flights with a dep_time
    filter(!is.na(dep_time))
  
  ,y = planes
  ,by = "tailnum"
) %>% 

  # get count of distinct tailnumbers
  summarise(n_distinct(tailnum))
```

```
## # A tibble: 1 × 1
##   `n_distinct(tailnum)`
##                   <int>
## 1                  3316
```


## Problem 7: Use the `nycflights13` to answer the following questions:

```         
-   What is the median arrival delay on a month-by-month basis in each airport?
-   For each airline, plot the median arrival delay for each month and origin airport.
```



```r
# //What is the median arrival delay on a month-by-month basis in each airport?

# filter on flights with dep_time
filter(flights, !is.na(dep_time)) %>% 
  
  # group by origin and month
  group_by(origin, month) %>% 
  
  # calculate median_arr_delay ignoring na using summarise
  summarise(median_arr_delay = median(arr_delay,na.rm=TRUE))
```

```
## `summarise()` has grouped output by 'origin'. You can override using the
## `.groups` argument.
```

```
## # A tibble: 36 × 3
## # Groups:   origin [3]
##    origin month median_arr_delay
##    <chr>  <int>            <dbl>
##  1 EWR        1                0
##  2 EWR        2               -2
##  3 EWR        3               -4
##  4 EWR        4               -1
##  5 EWR        5               -6
##  6 EWR        6               -1
##  7 EWR        7               -2
##  8 EWR        8               -5
##  9 EWR        9              -13
## 10 EWR       10               -6
## # ℹ 26 more rows
```

```r
# // For each airline, plot the median arrival delay for each month and origin airport.

# create plot of median_arr_delay by month with origin as color
ggplot(
  
  # filter on flights with dep_time
  filter(flights, !is.na(dep_time)) %>% 
    
    # group by carrier, origin and month
    group_by(carrier, origin, month) %>% 
    
    # calculate median_arr_delay ignoring na using summarise
    summarise(median_arr_delay = median(arr_delay,na.rm=TRUE))
  
  ,aes(
    x = month
    ,y = median_arr_delay
    ,color = origin
  )
) +
  
  # Add scatter
  geom_point(
) +
  
  # Facet wrap to dispay each carrier separately
  facet_wrap(~carrier
) +
  
  # Add labels and theme
  labs(
    x = "Month"
    ,y = "Median Arrival Delay"
    ,color = "Origin Airport"
    ,title = "Median Arrival Delay by Month by Origin Airport for each Carrier"
  ) +
  theme_light()
```

```
## `summarise()` has grouped output by 'carrier', 'origin'. You can override using
## the `.groups` argument.
```

<img src="/english/post/homework1_seanodoherty_files/figure-html/problem-7-1.png" width="672" />


## Problem 8: Let's take a closer look at what carriers service the route to San Francisco International (SFO). Join the `flights` and `airlines` tables and count which airlines flew the most to SFO. Produce a new dataframe, `fly_into_sfo` that contains three variables: the `name` of the airline, e.g., `United Air Lines Inc.` not `UA`, the count (number) of times it flew to SFO, and the `percent` of the trips that that particular airline flew to SFO.



```r
# store to new table
fly_into_sfo <-
  
  # join flights with dep_time to airlines to get full name
  left_join(
    x =filter(flights, !is.na(dep_time))
    ,y = airlines
    ,by = "carrier"
  ) %>% 
  
    # group by full name
    group_by(name) %>% 
    
    # get SFO count and PCT (divide by total count)
    summarise(
      count = sum(dest == "SFO")
      ,percent = sum(dest == "SFO") / n()
    ) %>% 
      
      # filter to only include those which fly to SFO
      filter(count > 0)


# display results
fly_into_sfo
```

```
## # A tibble: 5 × 3
##   name                   count percent
##   <chr>                  <int>   <dbl>
## 1 American Airlines Inc.  1402  0.0437
## 2 Delta Air Lines Inc.    1850  0.0387
## 3 JetBlue Airways         1029  0.0190
## 4 United Air Lines Inc.   6762  0.117 
## 5 Virgin America          2187  0.426
```


And here is some bonus ggplot code to plot your dataframe



```r
fly_into_sfo %>% 
  
  # sort 'name' of airline by the numbers it times to flew to SFO
  mutate(name = fct_reorder(name, count)) %>% 
  
  ggplot() +
  
  aes(x = count, 
      y = name) +
  
  # a simple bar/column plot
  geom_col() +
  
  # add labels, so each bar shows the % of total flights 
  geom_text(aes(label = percent),
             hjust = 1, 
             colour = "white", 
             size = 5)+
  
  # add labels to help our audience  
  labs(title="Which airline dominates the NYC to SFO route?", 
       subtitle = "as % of total flights in 2013",
       x= "Number of flights",
       y= NULL) +
  
  theme_minimal() + 
  
  # change the theme-- i just googled those , but you can use the ggThemeAssist add-in
  # https://cran.r-project.org/web/packages/ggThemeAssist/index.html
  
  theme(#
    # so title is left-aligned
    plot.title.position = "plot",
    
    # text in axes appears larger        
    axis.text = element_text(size=12),
    
    # title text is bigger
    plot.title = element_text(size=18)
      ) +

  # add one final layer of NULL, so if you comment out any lines
  # you never end up with a hanging `+` that awaits another ggplot layer
  NULL
```

<img src="/english/post/homework1_seanodoherty_files/figure-html/ggplot-flights-toSFO-1.png" width="672" />


## Problem 9: Let's take a look at cancellations of flights to SFO. We create a new dataframe `cancellations` as follows



```r
cancellations <- flights %>% 
  
  # just filter for destination == 'SFO'
  filter(dest == 'SFO') %>% 
  
  # a cancelled flight is one with no `dep_time` 
  filter(is.na(dep_time))
```


I want you to think how we would organise our data manipulation to create the following plot. No need to write the code, just explain in words how you would go about it.

*To obtain the plot below I would first group the data by month, carrier and origin and summarise the number of cancelled flights.*

*I would then create month_mmm by mutating the month field using month.abb.*

*I would then create a box plot with month_mmm on the x axis and cancelled flights on the y axis.*

*I would then use facet_grid to break the plot out by origin (cols) and carrier (rows).*

## Problem 10: On your own -- Hollywood Age Gap

The website https://hollywoodagegap.com is a record of *THE AGE DIFFERENCE IN YEARS BETWEEN MOVIE LOVE INTERESTS*. This is an informational site showing the age gap between movie love interests and the data follows certain rules:

-   The two (or more) actors play actual love interests (not just friends, coworkers, or some other non-romantic type of relationship)
-   The youngest of the two actors is at least 17 years old
-   No animated characters

The age gaps dataset includes "gender" columns, which always contain the values "man" or "woman". These values appear to indicate how the characters in each film identify and some of these values do not match how the actor identifies. We apologize if any characters are misgendered in the data!

The following is a data dictionary of the variables used

| variable            | class     | description                                                                                             |
|:-----------|:-----------|:----------------------------------------------|
| movie_name          | character | Name of the film                                                                                        |
| release_year        | integer   | Release year                                                                                            |
| director            | character | Director of the film                                                                                    |
| age_difference      | integer   | Age difference between the characters in whole years                                                    |
| couple_number       | integer   | An identifier for the couple in case multiple couples are listed for this film                          |
| actor_1\_name       | character | The name of the older actor in this couple                                                              |
| actor_2\_name       | character | The name of the younger actor in this couple                                                            |
| character_1\_gender | character | The gender of the older character, as identified by the person who submitted the data for this couple   |
| character_2\_gender | character | The gender of the younger character, as identified by the person who submitted the data for this couple |
| actor_1\_birthdate  | date      | The birthdate of the older member of the couple                                                         |
| actor_2\_birthdate  | date      | The birthdate of the younger member of the couple                                                       |
| actor_1\_age        | integer   | The age of the older actor when the film was released                                                   |
| actor_2\_age        | integer   | The age of the younger actor when the film was released                                                 |



```r
age_gaps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-14/age_gaps.csv')
```

```
## Rows: 1155 Columns: 13
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (6): movie_name, director, actor_1_name, actor_2_name, character_1_gend...
## dbl  (5): release_year, age_difference, couple_number, actor_1_age, actor_2_age
## date (2): actor_1_birthdate, actor_2_birthdate
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```


```r
# // How is age_difference distributed? What's the 'typical' age_difference in movies?

# Use boxplot to explore age_difference distribution
ggplot(
  age_gaps
  ,aes(y = age_difference)
) +
  geom_boxplot() +
  theme_light()
```

<img src="/english/post/homework1_seanodoherty_files/figure-html/problem-10-1.png" width="672" />

```r
# Use histogram to explore age_difference distribution
ggplot(
  age_gaps
  ,aes(x = age_difference)
) +
  geom_histogram() +
  theme_light()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

<img src="/english/post/homework1_seanodoherty_files/figure-html/problem-10-2.png" width="672" />

```r
# // How frequently does this rule apply in this dataset?

age_gaps %>% 
  
  # create fields for the older and younger actors ages from the two age fields
  mutate(
    older = if_else(actor_1_age > actor_2_age, actor_1_age, actor_2_age)
    ,younger = if_else(actor_1_age < actor_2_age, actor_1_age, actor_2_age)
  ) %>% 
  
  # Check if half age plus 7 holds
  mutate(half_age_plus_7 = if_else(younger >= ((older / 2) + 7), TRUE, FALSE)
  ) %>% 
  
  # group by whether the rule holds
  group_by(half_age_plus_7) %>% 
  
  # count instances
  summarise(n())
```

```
## # A tibble: 2 × 2
##   half_age_plus_7 `n()`
##   <lgl>           <int>
## 1 FALSE             326
## 2 TRUE              829
```

```r
# // Which movie has the greatest number of love interests?

# union the two lists together
union(
  
  # get lists of actors in each movie
  age_gaps %>% 
    group_by(movie_name, actor_name = actor_1_name) %>% 
    summarise()
  ,age_gaps %>% 
    group_by(movie_name, actor_name = actor_2_name) %>% 
    summarise()
) %>% 
  
  # group by movie
  group_by(movie_name) %>% 
  
  # count distinct actors
  summarise(actor_count = n()) %>% 
  
  # sort descending
  arrange(desc(actor_count))
```

```
## `summarise()` has grouped output by 'movie_name'. You can override using the
## `.groups` argument.
## `summarise()` has grouped output by 'movie_name'. You can override using the
## `.groups` argument.
```

```
## # A tibble: 830 × 2
##    movie_name                  actor_count
##    <chr>                             <int>
##  1 Love Actually                        13
##  2 The Family Stone                     10
##  3 He's Just Not That Into You           9
##  4 Mona Lisa Smile                       9
##  5 A Star Is Born                        8
##  6 American Pie                          8
##  7 Sex and the City                      8
##  8 Twilight                              8
##  9 Boogie Nights                         7
## 10 Soul Food                             7
## # ℹ 820 more rows
```

```r
# // Which actors/ actresses have the greatest number of love interests in this dataset?

# union the lists together to and keep duplicates 
union_all(
  
  # get list of actors 
  select(age_gaps, actor_name = actor_1_name)
  ,select(age_gaps, actor_name = actor_2_name)
) %>% 
  
  # group by actor
  group_by(actor_name) %>% 
  
  # count instances
  summarise(love_interests = n()) %>% 
  
  # sort descending
  arrange(desc(love_interests))
```

```
## # A tibble: 1,031 × 2
##    actor_name         love_interests
##    <chr>                       <int>
##  1 Keanu Reeves                   27
##  2 Adam Sandler                   20
##  3 Leonardo DiCaprio              17
##  4 Roger Moore                    17
##  5 Sean Connery                   17
##  6 Keira Knightley                14
##  7 Pierce Brosnan                 14
##  8 Harrison Ford                  13
##  9 Reese Witherspoon              13
## 10 Scarlett Johansson             13
## # ℹ 1,021 more rows
```

```r
# //Is the mean/median age difference staying constant over the years (1935 - 2022)?

# create plot
ggplot(
  
  # create combined table with release_year, metric and value to plot on same chart
  union(
    
    # get mean age difference by year
    age_gaps %>%
      group_by(release_year) %>% 
      summarize(
        metric = "Mean Age Difference"
        ,value = mean(age_difference)
      )
    
    # get median age difference by year
    ,age_gaps %>% 
      group_by(release_year) %>% 
      summarize(
        metric = "Median Age Difference"
        ,value = median(age_difference)
      )
  )
  
  # plot release year and value with metic as color
  ,aes(x = release_year, y = value, color = metric)
) +
  
  # add lines to chart
  geom_line() +
  theme_light()
```

<img src="/english/post/homework1_seanodoherty_files/figure-html/problem-10-3.png" width="672" />

```r
# // How frequently does Hollywood depict same-gender love interests?

age_gaps %>% 
  
  # Create column which returns TRUE when both characters are the same gender
  mutate(same_sex = if_else(character_1_gender == character_2_gender, TRUE, FALSE)) %>%
  
  # group by same sex test
  group_by(same_sex) %>% 
  
  # get counts
  summarize(n())
```

```
## # A tibble: 2 × 2
##   same_sex `n()`
##   <lgl>    <int>
## 1 FALSE     1132
## 2 TRUE        23
```


How would you explore this data set? Here are some ideas of tables/ graphs to help you with your analysis

-   How is `age_difference` distributed? What's the 'typical' `age_difference` in movies?

-   The `half plus seven\` rule. Large age disparities in relationships carry certain stigmas. One popular rule of thumb is the [half-your-age-plus-seven](https://en.wikipedia.org/wiki/Age_disparity_in_sexual_relationships#The_.22half-your-age-plus-seven.22_rule) rule. This rule states you should never date anyone under half your age plus seven, establishing a minimum boundary on whom one can date. In order for a dating relationship to be acceptable under this rule, your partner's age must be:

`$$\frac{\text{Your age}}{2} + 7 < \text{Partner Age} < (\text{Your age} - 7) * 2$$` How frequently does this rule apply in this dataset?

-   Which movie has the greatest number of love interests?
-   Which actors/ actresses have the greatest number of love interests in this dataset?
-   Is the mean/median age difference staying constant over the years (1935 - 2022)?
-   How frequently does Hollywood depict same-gender love interests?

# Deliverables

There is a lot of explanatory text, comments, etc. You do not need these, so delete them and produce a stand-alone document that you could share with someone. Render the edited and completed Quarto Markdown (qmd) file as a Word document (use the "Render" button at the top of the script editor window) and upload it to Canvas. You must be commiting and pushing tour changes to your own Github repo as you go along.

# Details

-   Who did you collaborate with: *na*
-   Approximately how much time did you spend on this problem set: *a few hours*
-   What, if anything, gave you the most trouble: *researching functions*

**Please seek out help when you need it,** and remember the [15-minute rule](https://mam2022.netlify.app/syllabus/#the-15-minute-rule){target="_blank"}. You know enough R (and have enough examples of code from class and your readings) to be able to do this. If you get stuck, ask for help from others, post a question on Slack-- and remember that I am here to help too!

> As a true test to yourself, do you understand the code you submitted and are you able to explain it to someone else?

# Rubric

13/13: Problem set is 100% completed. Every question was attempted and answered, and most answers are correct. Code is well-documented (both self-documented and with additional comments as necessary). Used tidyverse, instead of base R. Graphs and tables are properly labelled. Analysis is clear and easy to follow, either because graphs are labeled clearly or you've written additional text to describe how you interpret the output. Multiple Github commits. Work is exceptional. I will not assign these often.

8/13: Problem set is 60--80% complete and most answers are correct. This is the expected level of performance. Solid effort. Hits all the elements. No clear mistakes. Easy to follow (both the code and the output). A few Github commits.

5/13: Problem set is less than 60% complete and/or most answers are incorrect. This indicates that you need to improve next time. I will hopefully not assign these often. Displays minimal effort. Doesn't complete all components. Code is poorly written and not documented. Uses the same type of plot for each graph, or doesn't use plots appropriate for the variables being analyzed. No Github commits.

