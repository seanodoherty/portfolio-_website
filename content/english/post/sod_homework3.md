---
title: 'Homework 3: Databases, web scraping, and a basic Shiny app'
author: "Seán O'Doherty"
date: "2023-06-18"
output:
  html_document:
    theme: flatly
    highlight: zenburn
    number_sections: yes
    toc: yes
    toc_float: yes
    code_folding: show
  pdf_document:
    toc: yes
  word_document:
    toc: yes
---



# Money in UK politics

[The Westminster Accounts](https://news.sky.com/story/the-westminster-accounts-12786091), a recent collaboration between Sky News and Tortoise Media, examines the flow of money through UK politics. It does so by combining data from three key sources:

1.  [Register of Members' Financial Interests](https://www.parliament.uk/mps-lords-and-offices/standards-and-financial-interests/parliamentary-commissioner-for-standards/registers-of-interests/register-of-members-financial-interests/),
2.  [Electoral Commission records of donations to parties](http://search.electoralcommission.org.uk/English/Search/Donations), and
3.  [Register of All-Party Parliamentary Groups](https://www.parliament.uk/mps-lords-and-offices/standards-and-financial-interests/parliamentary-commissioner-for-standards/registers-of-interests/register-of-all-party-party-parliamentary-groups/).

You can [search and explore the results](https://news.sky.com/story/westminster-accounts-search-for-your-mp-or-enter-your-full-postcode-12771627) through the collaboration's interactive database. Simon Willison [has extracted a database](https://til.simonwillison.net/shot-scraper/scraping-flourish) and this is what we will be working with. If you want to read more about [the project's methodology](https://www.tortoisemedia.com/2023/01/08/the-westminster-accounts-methodology/).

## Open a connection to the database

The database made available by Simon Willison is an `SQLite` database


```r
sky_westminster <- DBI::dbConnect(
  drv = RSQLite::SQLite(),
  dbname = here::here("data", "sky-westminster-files.db")
)
```

How many tables does the database have? *The database has 7 tables*


```r
DBI::dbListTables(sky_westminster)
```

```
## [1] "appg_donations"  "appgs"           "member_appgs"    "members"        
## [5] "parties"         "party_donations" "payments"
```

## Which MP has received the most amount of money?

You need to work with the `payments` and `members` tables and for now we just want the total among all years. To insert a new, blank chunk of code where you can write your beautiful code (and comments!), please use the following shortcut: `Ctrl + Alt + I` (Windows) or `cmd + option + I` (mac)


```r
# get tables as tibbles
payments <- dplyr::tbl(sky_westminster, "payments")
members <- dplyr::tbl(sky_westminster, "members")

# get payment count by member_id
payment_count_value_member <-
  payments %>% 
    group_by(member_id) %>%
    summarise(
      payment_count = n()
      ,value = sum(value)
    )

# join to members to get memebr name
left_join(
  x = payment_count_value_member
  ,y = select(members, id, name)
  ,by = c("member_id" = "id")
) %>% 
  arrange(desc(value))
```

```
## Warning: Missing values are always removed in SQL aggregation functions.
## Use `na.rm = TRUE` to silence this warning
## This warning is displayed once every 8 hours.
```

```
## # Source:     SQL [?? x 4]
## # Database:   sqlite 3.41.2 [C:\Users\sodoh\Documents\LBS\2023 Summer\E628 Data Science\portfolio_website\data\sky-westminster-files.db]
## # Ordered by: desc(value)
##    member_id payment_count    value name            
##    <chr>             <int>    <dbl> <chr>           
##  1 m8                   78 2809765. Theresa May     
##  2 m1508                50 2191387. Sir Geoffrey Cox
##  3 m1423                56 1282402  Boris Johnson   
##  4 m4514               104  799936. Keir Starmer    
##  5 m1211                66  769373. Andrew Mitchell 
##  6 m3958                32  712321. Fiona Bruce     
##  7 m14                  49  692438. John Redwood    
##  8 m4483                37  546043  Rishi Sunak     
##  9 m4097                30  538678. Liz Truss       
## 10 m188                 88  441681. Ed Davey        
## # ℹ more rows
```

## Any `entity` that accounts for more than 5% of all donations?

Is there any `entity` whose donations account for more than 5% of the total payments given to MPs over the 2020-2022 interval? Who are they and who did they give money to?


```r
# assign to table to use later
entity_greater_than_5pct <-

  payments %>% 
    
    #group by entity
    group_by(entity) %>% 
    
    # get sum of entity value
    summarise(value = sum(value)) %>% 
      
    # calculate pct of total value
    mutate(value_pct = 100 * value / sum(value)) %>% 
  
    # filter for percent of total value greater than 5
    filter(value_pct >= 5)

entity_greater_than_5pct
```

```
## # Source:   SQL [1 x 3]
## # Database: sqlite 3.41.2 [C:\Users\sodoh\Documents\LBS\2023 Summer\E628 Data Science\portfolio_website\data\sky-westminster-files.db]
##   entity         value value_pct
##   <chr>          <dbl>     <dbl>
## 1 Withers LLP 1812732.      5.25
```

```r
# left join to members to get member name
left_join(
  
  x = 
  # use semi join to keep payments which are in the entity_greater_than_5pct table
  semi_join(
    x = payments
    ,y = entity_greater_than_5pct
    ,by = "entity"
  )
  ,y = members
  ,by = c("member_id" = "id")
) %>% 
  
  # group by entity and name
  group_by(entity, name) %>% 
  
  # summarise to get sum of value
  summarise(value = sum(value))
```

```
## # Source:   SQL [1 x 3]
## # Database: sqlite 3.41.2 [C:\Users\sodoh\Documents\LBS\2023 Summer\E628 Data Science\portfolio_website\data\sky-westminster-files.db]
## # Groups:   entity
##   entity      name                value
##   <chr>       <chr>               <dbl>
## 1 Withers LLP Sir Geoffrey Cox 1812732.
```

## Do `entity` donors give to a single party or not?

-   How many distinct entities who paid money to MPS are there?
-   How many (as a number and %) donated to MPs belonging to a single party only?

payments


```r
# use summarise and count to get the distinct count of entities
payments %>% 
  summarise(count(entity))
```

```
## # Source:   SQL [1 x 1]
## # Database: sqlite 3.41.2 [C:\Users\sodoh\Documents\LBS\2023 Summer\E628 Data Science\portfolio_website\data\sky-westminster-files.db]
##   `count(entity)`
##             <int>
## 1           10539
```

```r
# left join to get party_id
left_join(
  x = payments
  ,y = members
  ,by = c("member_id" = "id")
) %>% 
  
  # group by entity
  group_by(entity) %>%
  
  # count distinct party_id
  summarise(party_count = count(party_id)) %>% 
  
  # create field identifying entities donating to multiple parties
  mutate(single_party = ifelse(party_count ==  1, "Single party", "Multi party")) %>%
  # group by single / multi party entities
  group_by(single_party) %>% 
  
  # count entities
  summarise(entity_count = count(entity)) %>%
  
  # get percentage
  mutate(entity_count_pct = 100 * as.numeric(entity_count) / sum(entity_count))
```

```
## # Source:   SQL [2 x 3]
## # Database: sqlite 3.41.2 [C:\Users\sodoh\Documents\LBS\2023 Summer\E628 Data Science\portfolio_website\data\sky-westminster-files.db]
##   single_party entity_count entity_count_pct
##   <chr>               <int>            <dbl>
## 1 Multi party           852             38.5
## 2 Single party         1361             61.5
```

## Which party has raised the greatest amount of money in each of the years 2020-2022?

I would like you to write code that generates the following table.


```r
knitr::include_graphics(here::here("images", "total_donations_table.png"), error = FALSE)
```

![](../../../images/total_donations_table.png)<!-- -->

```r
parties <- dplyr::tbl(sky_westminster, "parties")

# year / party name / donations / prop of year

# count(payments, category_name)

# store base data to table
year_party_donations <-
  
  # join donations from payments table to member / party
  left_join(
    x = payments # filter(payments, category_name == "Cash donations")
    ,y = 
      left_join(
        x = members
        ,y = parties
        ,by = c("party_id" = "id")
      )
    ,by = c("member_id" = "id")
  ) %>%
    
    # rename name fields for clarity
    rename(
      member_name = name.x
      ,party_name = name.y
      ) %>% 
    
    # Extract year from date field
    mutate(year = str_sub(date, -4, -1)) %>% 
  
    # filter to 2020-2022
    filter(year %in% c("2020", "2021", "2022")) %>% 
    
    # Group by year and party_name and sum value
    group_by(year, party_name) %>%
    summarise(donations = sum(value))

# store subotals to table
year_total_donations <-
  
  # get yearly sub-totals
  year_party_donations %>% 
    group_by(year) %>% 
    summarise(year_total_donations = sum(donations))

# store output to table
year_party_value_prop <-
  
  # left join base data to yearly totals  
  left_join(
    x = year_party_donations
    ,y = year_total_donations
    ,by = "year"
  ) %>% 
  
    # calculate prop
    mutate(prop = donations / year_total_donations) %>% 
    
    # use select to format output
    select(
      year
      ,name = party_name
      ,total_year_donations = donations
      ,prop
    )

# display output
year_party_value_prop
```

```
## # Source:   SQL [?? x 4]
## # Database: sqlite 3.41.2 [C:\Users\sodoh\Documents\LBS\2023 Summer\E628 Data Science\portfolio_website\data\sky-westminster-files.db]
## # Groups:   year
##    year  name                      total_year_donations     prop
##    <chr> <chr>                                    <dbl>    <dbl>
##  1 2020  Alba Party                               1320  0.000125
##  2 2020  Conservative                          6035344. 0.571   
##  3 2020  Democratic Unionist Party                5715. 0.000540
##  4 2020  Green Party                              9500  0.000898
##  5 2020  Independent                            230103. 0.0218  
##  6 2020  Labour                                3615844. 0.342   
##  7 2020  Liberal Democrats                      537694  0.0508  
##  8 2020  Plaid Cymru                             23072  0.00218 
##  9 2020  Scottish National Party                108599. 0.0103  
## 10 2020  Sinn Féin                                1911  0.000181
## # ℹ more rows
```

... and then, based on this data, plot the following graph.


```r
knitr::include_graphics(here::here("images", "total_donations_graph.png"), error = FALSE)
```

![](../../../images/total_donations_graph.png)<!-- -->

```r
options(dplyr.summarise.inform = FALSE)

# create table of top 10 parties by donation
party_value_top10 <-
  year_party_value_prop %>% 
  
    # filter out independent
    filter(name != "Independent") %>% 
    
    # group by party
    group_by(name) %>% 
    
    # sum up donations
    summarise(total_donations = sum(total_year_donations)) %>%
    
    # keep only top 10
    slice_max(n = 10, total_donations)

# semi join to top10
semi_join(
  x = year_party_value_prop
  ,y = party_value_top10
  ,by = "name"
) %>%
  # add plot
  ggplot() +
  
  # set aesthetics
  aes(
    x = factor(year)
    ,y = total_year_donations
    
    # set fil order using fct_reorder and fct_rev
    ,fill = fct_rev(fct_reorder(name, total_year_donations))
  ) +
  
  # add bar chart
  geom_col(
    
    # make bars clustered instead of stacked
    position = "dodge"
  ) +
  
  # apply there and set text size
  theme_light(
    base_size = 8
  ) +
  
  # set labels
  labs(
    x = NULL
    ,y = NULL
    ,title = "Conservatives have captured the majority of political donations"
    ,subtitle = "Donations to political parties, 2020-2022"
    ,fill = "Party"
  ) +
  
  # format numbers on y axis
  scale_y_continuous(
    labels = comma
  )
```

<img src="/english/post/sod_homework3_files/figure-html/unnamed-chunk-8-2.png" width="672" />

This uses the default ggplot colour pallete, as I dont want you to worry about using the [official colours for each party](https://en.wikipedia.org/wiki/Wikipedia:Index_of_United_Kingdom_political_parties_meta_attributes). However, I would like you to ensure the parties are sorted according to total donations and not alphabetically. You may even want to remove some of the smaller parties that hardly register on the graph. Would facetting help you?

Finally, when you are done working with the databse, make sure you close the connection, or disconnect from the database.


```r
dbDisconnect(sky_westminster)
```

# Anonymised Covid patient data from the CDC

We will be using a dataset with [anonymous Covid-19 patient data that the CDC publishes every month](https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data-with-Ge/n8mc-b4w4). The file we will use was released on April 11, 2023, and has data on 98 million of patients, with 19 features. This file cannot be loaded in memory, but luckily we have the data in `parquet` format and we will use the `{arrow}` package.

## Obtain the data

The dataset `cdc-covid-geography` in in `parquet` format that {arrow}can handle. It is \> 600Mb and too large to be hosted on Canvas or Github, so please download it from dropbox <https://www.dropbox.com/sh/q1yk8mmnbbrzavl/AAAxzRtIhag9Nc_hODafGV2ka?dl=0> and save it in your `dsb` repo, under the `data` folder


```
## 0.03 sec elapsed
```

```
## FileSystemDataset with 1 Parquet file
## 97,799,772 rows x 19 columns
## $ case_month                     <string> "2021-09", "2022-09", "2022-01", "2020…
## $ res_state                      <string> "TX", "TX", "TX", "CA", "IL", "CA", "N…
## $ state_fips_code                 <int32> 48, 48, 48, 6, 17, 6, 36, 36, 36, 53, …
## $ res_county                     <string> "TARRANT", NA, "HARRIS", "SAN BERNARDI…
## $ county_fips_code                <int32> 48439, NA, 48201, 6071, 17031, 6085, 3…
## $ age_group                      <string> "18 to 49 years", "18 to 49 years", "1…
## $ sex                            <string> "Male", "Male", "Female", "Female", "F…
## $ race                           <string> "White", "White", "Unknown", "Asian", …
## $ ethnicity                      <string> "Non-Hispanic/Latino", "Non-Hispanic/L…
## $ case_positive_specimen_interval <int32> NA, NA, NA, NA, 0, NA, 0, 0, 0, 0, 0, …
## $ case_onset_interval             <int32> NA, NA, -1, NA, 0, NA, NA, NA, NA, 0, …
## $ process                        <string> "Missing", "Missing", "Missing", "Miss…
## $ exposure_yn                    <string> "Missing", "Missing", "Missing", "Miss…
## $ current_status                 <string> "Laboratory-confirmed case", "Probable…
## $ symptom_status                 <string> "Missing", "Missing", "Symptomatic", "…
## $ hosp_yn                        <string> "Missing", "Missing", "No", "No", "No"…
## $ icu_yn                         <string> "Missing", "Missing", "Missing", "Miss…
## $ death_yn                       <string> "Missing", "Missing", "Missing", "Miss…
## $ underlying_conditions_yn       <string> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
```

Can you query the database and replicate the following plot?


```r
knitr::include_graphics(here::here("images", "covid-CFR-ICU.png"), error = FALSE)
```

![](../../../images/covid-CFR-ICU.png)<!-- -->

```r
# store results to local tibble
cdc_basedata_001 <-
  
  cdc_data %>% 
    
    # identify and group by relevant cols
    group_by(
      age_group
      ,sex
      ,icu_yn
      ,death_yn
    ) %>% 
    
    # get count
    summarise(
      case_count = n()
    ) %>% 
    
  # collect data from connection
  collect()

cdc_basedata_001 %>%
  
  #clean data for chart
  filter(
    !age_group %in% c("Missing", NA)
    ,sex %in% c("Female", "Male")
    ,icu_yn %in% c("No", "Yes")
  ) %>% 
  
  # group by non death factors and calculate cfs
  group_by(
    age_group
    ,sex
    ,icu_yn
  )  %>% 
  summarise(
    death_count = sum(case_count[death_yn == "Yes"], na.rm = TRUE)
    ,case_count = sum(case_count)
    ,cfr_pct = round(100 * death_count / case_count, 0)
  ) %>%
  
  # Change yn
  mutate(
    icu_yn = case_when(
      icu_yn == "Yes" ~ "ICU Admission"
      ,icu_yn == "No" ~ "No ICU Admission"
    )
  ) %>% 

  # add plot
  ggplot() +
  
  # add aesthetics
  aes(
    y = cfr_pct / 100
    ,x = age_group
    ,fill = "orange"
  ) +
  
  # add bar
  geom_col(
    show.legend =FALSE
  ) +
  
  # flip axes
  coord_flip() +
  
  # apply theme and adjust font size
  theme_light(
    base_size = 6
  ) +
  
  # set labels
  labs(
    x = NULL
    ,y = NULL
    ,title = "Covid CFR % by age group, sex and ICU Admission"
  ) +
  geom_text(
    aes(
      label = cfr_pct
      ,hjust = 1.2
    )
  ) +
  
  # apply facet grid
  facet_grid(
    rows = vars(icu_yn)
    ,cols = vars(sex)
  ) +
  
  # alter axis labels
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  )
```

<img src="/english/post/sod_homework3_files/figure-html/unnamed-chunk-11-2.png" width="672" />

The previous plot is an aggregate plot for all three years of data. What if we wanted to plot Case Fatality Ratio (CFR) over time? Write code that collects the relevant data from the database and plots the following


```r
knitr::include_graphics(here::here("images", "cfr-icu-overtime.png"), error = FALSE)
```

![](../../../images/cfr-icu-overtime.png)<!-- -->

```r
# store results to local tibble
cdc_basedata_002 <-
  
  cdc_data %>%
    
    # identify and group by relevant cols
    group_by(
      case_month
      ,age_group
      ,sex
      ,icu_yn
      ,death_yn
    ) %>% 
    
    # get counts
    summarise(
      case_count = n()
    ) %>% 
    
  # collect data from connection
  collect()

cdc_basedata_002 %>%
  
  #clean data for chart
  filter(
    !age_group %in% c("Missing", NA, "0 - 17 years")
    ,sex %in% c("Female", "Male")
    ,icu_yn %in% c("No", "Yes")
  ) %>% 
  
  # group by non death factors and calculate cfs
  group_by(
    case_month
    ,age_group
    ,sex
    ,icu_yn
  ) %>% 
  summarise(
    death_count = sum(case_count[death_yn == "Yes"], na.rm = TRUE)
    ,case_count = sum(case_count)
    ,cfr_pct = round(100 * death_count / case_count, 0)
  ) %>%
  
  # Change yn
  mutate(
    icu_yn = case_when(
      icu_yn == "Yes" ~ "ICU Admission"
      ,icu_yn == "No" ~ "No ICU Admission"
    )
  ) %>%
  
  # add plot
  ggplot() +
  
  # add aesthetics
  aes(
    y = cfr_pct / 100
    ,x = case_month
    ,color = age_group
    ,group = age_group
  ) +
  
  # add line
  geom_line() +
  
  # apply theme and adjust font size
  theme_light(
    base_size = 6
  ) +
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
  ) +
  
  # set labels
  labs(
    x = NULL
    ,y = NULL
    ,title = "Covid CFR % by age group, sex and ICU Admission"
  ) +
  geom_text(
    data = . %>% filter(cfr_pct != 0)
    ,aes(
      label = cfr_pct
      ,hjust = -0.4
    )
    ,size = 2
  ) +
  
  # apply facet grid
  facet_grid(
    rows = vars(icu_yn)
    ,cols = vars(sex)
  ) +
  
  # alter axis labels
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  )
```

<img src="/english/post/sod_homework3_files/figure-html/unnamed-chunk-12-2.png" width="672" />

For each patient, the dataframe also lists the patient's states and county [FIPS code](https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code). The CDC also has information on the [NCHS Urban-Rural classification scheme for counties](https://www.cdc.gov/nchs/data_access/urban_rural.htm)


```r
urban_rural <- read_xlsx(here::here("data", "NCHSURCodes2013.xlsx")) %>% 
  janitor::clean_names() 

glimpse(urban_rural)
```

```
## Rows: 3,149
## Columns: 9
## $ fips_code        <dbl> 1001, 1003, 1005, 1007, 1009, 1011, 1013, 1015, 1017,…
## $ state_abr        <chr> "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL",…
## $ county_name      <chr> "Autauga County", "Baldwin County", "Barbour County",…
## $ cbsa_title       <chr> "Montgomery, AL", "Daphne-Fairhope-Foley, AL", NA, "B…
## $ cbsa_2012_pop    <chr> "377149", "190790", ".", "1136650", "1136650", ".", "…
## $ county_2012_pop  <chr> "55514", "190790", "27201", "22597", "57826", "10474"…
## $ x2013_code       <dbl> 3, 4, 6, 2, 2, 6, 6, 4, 5, 6, 2, 6, 6, 6, 6, 5, 4, 6,…
## $ x2006_code       <dbl> 3, 5, 5, 2, 2, 6, 6, 4, 5, 6, 2, 6, 6, 6, 6, 5, 4, 6,…
## $ x1990_based_code <chr> "3", "3", "5", "6", "3", "6", "6", "4", "6", "6", "6"…
```

Each county belongs in seix diffent categoreis, with categories 1-4 being urban areas and categories 5-6 being rural, according to the following criteria captured in `x2013_code`

Category name

1.  Large central metro - 1 million or more population and contains the entire population of the largest principal city
2.  large fringe metro - 1 million or more poulation, but does not qualify as 1
3.  Medium metro - 250K - 1 million population
4.  Small metropolitan population \< 250K
5.  Micropolitan
6.  Noncore

Can you query the database, extract the relevant information, and reproduce the following two graphs that look at the Case Fatality ratio (CFR) in different counties, according to their population?


```r
knitr::include_graphics(here::here("images", "cfr-county-population.png"), error = FALSE)
```

![](../../../images/cfr-county-population.png)<!-- -->

```r
# store results to local tibble
cdc_basedata_003 <-
  
  cdc_data %>%
    
    # identify and group by relevant cols
    group_by(
      case_month
      ,county_fips_code
      ,death_yn
    ) %>% 
    
    # get case count
    summarise(
      case_count = n()
    ) %>% 
    
  # collect data from connection
  collect()

# overwrite
 cdc_basedata_003 <-
  
  # join to urban_rural
  inner_join(
    x = cdc_basedata_003
    ,y = select(urban_rural, fips_code, x2013_code)
    ,by = c("county_fips_code" = "fips_code")
  )

cdc_basedata_003 %>%

  # Categorise
  mutate(
    category = case_when(
      x2013_code == 1 ~ "1. Large central metro"
      ,x2013_code == 2 ~ "2. large fringe metro"
      ,x2013_code == 3 ~ "3. Medium metro"
      ,x2013_code == 4 ~ "4. Small metropolitan"
      ,x2013_code == 5 ~ "5. Micropolitan"
      ,x2013_code == 6 ~ "6. Noncore"
    )
  ) %>% 

  # group by non death factors and calculate cfs
  group_by(
    case_month
    ,category
  ) %>%
  summarise(
    death_count = sum(case_count[death_yn == "Yes"], na.rm = TRUE)
    ,case_count = sum(case_count)
    ,cfr_pct = round(100 * death_count / case_count, 1)
  ) %>%

  # add plot
  ggplot() +

  # add aesthetics
  aes(
    y = cfr_pct / 100
    ,x = case_month
    ,color = category
    ,group = category
  ) +

  # add line
  geom_line(linewidth = 0.2) +

  # apply theme and adjust font size
  theme_light(
    base_size = 6
  ) +
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
  ) +

  # set labels
  labs(
    x = NULL
    ,y = NULL
    ,title = "Covid CFR % by county population"
  ) +
  geom_text(
    data = . %>% filter(cfr_pct != 0)
    ,aes(
      label = cfr_pct
      ,hjust = -0.4
      ,vjust = -0.2
    )
    ,size = 2
  ) +
  guides(
    color = FALSE
  ) +

  # apply facet wrap
  facet_wrap(
    ~ category
    ,nrow = 3
    ,scales = "free"
  ) +

  # alter axis labels
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  )
```

```
## Warning: The `<scale>` argument of `guides()` cannot be `FALSE`. Use "none" instead as
## of ggplot2 3.3.4.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

<img src="/english/post/sod_homework3_files/figure-html/unnamed-chunk-14-2.png" width="672" />


```r
knitr::include_graphics(here::here("images", "cfr-rural-urban.png"), error = FALSE)
```

![](../../../images/cfr-rural-urban.png)<!-- -->

```r
# store results to local tibble
cdc_basedata_004 <-
  
  cdc_data %>%
    
    # filter out 2020-01
    filter(
      case_month != "2020-01"
    ) %>% 
    
    # identify and group by relevant cols
    group_by(
      case_month
      ,county_fips_code
      ,death_yn
    ) %>% 
    
    # get case count
    summarise(
      case_count = n()
    ) %>% 
    
  # collect data from connection
  collect()

# overwrite
 cdc_basedata_004 <-
  
  # join to urban_rural
  inner_join(
    x = cdc_basedata_004
    ,y = select(urban_rural, fips_code, x2013_code)
    ,by = c("county_fips_code" = "fips_code")
  )

cdc_basedata_004 %>%

  # Categorise
  mutate(
    category = case_when(
      x2013_code %in% c(1, 2, 3, 4) ~ "Urban"
      ,x2013_code %in% c(5, 6) ~ "Rural"
    )
  ) %>%

  # group by non death factors and calculate cfs
  group_by(
    case_month
    ,category
  ) %>%
  summarise(
    death_count = sum(case_count[death_yn == "Yes"], na.rm = TRUE)
    ,case_count = sum(case_count)
    ,cfr_pct = round(100 * death_count / case_count, 1)
  ) %>%

  # add plot
  ggplot() +

  # add aesthetics
  aes(
    y = cfr_pct / 100
    ,x = case_month
    ,color = category
    ,group = category
  ) +

  # add line
  geom_line(linewidth = 0.2) +

  # apply theme and adjust font size
  theme_light(
    base_size = 6
  ) +
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
  ) +

  # set labels
  labs(
    x = NULL
    ,y = NULL
    ,title = "Covid CFR % by Urban vs Rural"
    ,color = "Counties"
  ) +
  geom_text(
    data = . %>% filter(cfr_pct != 0)
    ,aes(
      label = cfr_pct
      ,hjust = -0.4
      ,vjust = -0.2
    )
    ,size = 2
    ,color = "black"
  ) +

  # alter axis labels
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  )
```

<img src="/english/post/sod_homework3_files/figure-html/unnamed-chunk-15-2.png" width="672" />

# Money in US politics

In the United States, [*"only American citizens (and immigrants with green cards) can contribute to federal politics, but the American divisions of foreign companies can form political action committees (PACs) and collect contributions from their American employees."*](https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs)

We will scrape and work with data foreign connected PACs that donate to US political campaigns. The data for foreign connected PAC contributions in the 2022 election cycle can be found at <https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs/2022>. Then, we will use a similar approach to get data such contributions from previous years so that we can examine trends over time.

All data come from [OpenSecrets.org](https://www.opensecrets.org), a *"website tracking the influence of money on U.S. politics, and how that money affects policy and citizens' lives"*.


```r
library(robotstxt)
paths_allowed("https://www.opensecrets.org")
```

```
## [1] TRUE
```

```r
base_url <- "https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs/2022"

contributions_tables <- base_url %>%
  read_html()
```

-   First, make sure you can scrape the data for 2022. Use janitor::clean_names() to rename variables scraped using `snake_case` naming.

    
    ```r
    library(janitor)
    ```
    
    ```
    ## 
    ## Attaching package: 'janitor'
    ```
    
    ```
    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test
    ```
    
    ```r
    # create funtion to use later
    scrape_contributions_table <- function(html){
      
      # store tables
      tables <-
        html %>% 
          html_nodes(css = "table") %>% 
          html_table()
      
      # save table 1 to df
      table <-
        tables[[1]] %>% 
        clean_names()
      
      # return df
      table
    
    }
    
    # use base_url to check if it works
    contributions <-
      scrape_contributions_table(contributions_tables)
    
    contributions
    ```
    
    ```
    ## # A tibble: 215 × 5
    ##    pac_name_affiliate                  country_of_origin_pa…¹ total dems  repubs
    ##    <chr>                               <chr>                  <chr> <chr> <chr> 
    ##  1 Accenture (Accenture)               Ireland/Accenture plc  $3,0… $0    $3,000
    ##  2 Acreage Holdings                    Canada/Acreage Holdin… $0    $0    $0    
    ##  3 Air Liquide America                 France/L'Air Liquide … $17,… $14,… $2,500
    ##  4 Airbus Group                        Netherlands/Airbus Gr… $193… $82,… $111,…
    ##  5 Alexion Pharmaceuticals (AstraZene… UK/AstraZeneca PLC     $186… $104… $82,2…
    ##  6 Alkermes Inc                        Ireland/Alkermes Plc   $84,… $34,… $50,0…
    ##  7 Allianz of America (Allianz)        Germany/Allianz AG Ho… $31,… $20,… $11,0…
    ##  8 AMG Vanadium                        Netherlands/AMG Advan… $2,5… $0    $2,525
    ##  9 Anheuser-Busch (Anheuser-Busch InB… Belgium/Anheuser-Busc… $457… $218… $239,…
    ## 10 AON Corp (AON plc)                  UK/AON PLC             $98,… $52,… $46,5…
    ## # ℹ 205 more rows
    ## # ℹ abbreviated name: ¹​country_of_origin_parent_company
    ```

Clean the data:

-   Write a function that converts contribution amounts in `total`, `dems`, and `repubs` from character strings to numeric values.
-   Separate the `country_of_origin_parent_company` into two such that country and parent company appear in different columns for country-level analysis.


```r
# write a function to parse_currency
parse_currency <- function(x){
  x %>%
    
    # remove dollar signs
    str_remove("\\$") %>%
    
    # remove all occurrences of commas
    str_remove_all(",") %>%
    
    # convert to numeric
    as.numeric()
}

# clean country/parent co and contributions 
contributions <- contributions %>%
  separate(country_of_origin_parent_company, 
           into = c("country", "parent"), 
           sep = "/", 
           extra = "merge") %>%
  mutate(
    total = parse_currency(total),
    dems = parse_currency(dems),
    repubs = parse_currency(repubs)
  )
```

-   Write a function called `scrape_pac()` that scrapes information from the Open Secrets webpage for foreign-connected PAC contributions in a given year. This function should

    -   have one input: the URL of the webpage and should return a data frame.
    -   add a new column to the data frame for `year`. We will want this information when we ultimately have data from all years, so this is a good time to keep track of it. Our function doesn't take a year argument, but the year is embedded in the URL, so we can extract it out of there, and add it as a new column. Use the `str_sub()` function to extract the last 4 characters from the URL. You will probably want to look at the help for this function to figure out how to specify "last 4 characters".

-   Define the URLs for 2022, 2020, and 2000 contributions. Then, test your function using these URLs as inputs. Does the function seem to do what you expected it to do?

-   Construct a vector called `urls` that contains the URLs for each webpage that contains information on foreign-connected PAC contributions for a given year.

-   Map the `scrape_pac()` function over `urls` in a way that will result in a data frame called `contributions_all`.

-   Write the data frame to a csv file called `contributions-all.csv` in the `data` folder.


```r
scrape_pac <- function(url){
  
  contributions_tables <-
    url %>%
    read_html()
  
  contributions <-
    scrape_contributions_table(contributions_tables)

  contributions <-
    contributions %>%
      separate(country_of_origin_parent_company,
               into = c("country", "parent"),
               sep = "/",
               extra = "merge") %>%
      mutate(
        total = parse_currency(total)
        ,dems = parse_currency(dems)
        ,repubs = parse_currency(repubs)
        ,year = str_sub(url, -4, -1)
  )
  
  contributions
}

urls = c(
  "https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs/2022"
  ,"https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs/2021"
  ,"https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs/2020"
  ,"https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs/2019"
  
)

contributions_all <- map_df(urls, scrape_pac)

write.csv(contributions_all, here::here("data", "contributions_all.csv"), row.names=FALSE)
```

# Scraping consulting jobs

The website [https://www.consultancy.uk/jobs/](https://www.consultancy.uk/jobs) lists job openings for consulting jobs.


```r
library(robotstxt)
paths_allowed("https://www.consultancy.uk") #is it ok to scrape?
```

```
## 
 www.consultancy.uk
```

```
## [1] TRUE
```

```r
base_url <- "https://www.consultancy.uk/jobs/page/1"

# store as function to use again
scrape_listings_table <- function(base_url){
  
  listings_html <- base_url %>%
    read_html()
  
  # store tables
  tables <-
    listings_html %>% 
      html_nodes(css = "table") %>% 
      html_table()
  
  # save table 1 to df
  table <-
    tables[[1]] %>% 
    clean_names()
  
  # definte output
  return(table)
}

# return df
scrape_listings_table(base_url)
```

```
## # A tibble: 45 × 4
##    job                                               firm  functional_area type 
##    <chr>                                             <chr> <chr>           <chr>
##  1 "Consultant Treasury Technology\nZanders"         Zand… "Corporate Fin… Job  
##  2 "Director - Supply Chain Strategy & Transformati… Capg… "Strategy\n+1\… Job  
##  3 "Senior Consultant | Energy & Natural Resources … FTI … "Unknown"       Job  
##  4 "Business Analyst\nHumatica"                      Huma… "Data Science"  Job  
##  5 "Data Scientist\nDigital Power"                   Digi… "Data Science"  Job  
##  6 "Senior Analyst\nCIL Management Consultants"      CIL … "Strategy\n+1\… Job  
##  7 "Consultant Roles (at all levels) – IT Advisory\… Maso… "Digital\n+4\n… Job  
##  8 "Healthcare consultant\nDevelop Consulting"       Deve… "Lean & SixSig… Job  
##  9 "Senior Strategist\nThe Upside"                   The … "Strategy\n+2\… Job  
## 10 "Intermediate Quantity Surveyor\nPanoptic Consul… Pano… "Project Manag… Job  
## # ℹ 35 more rows
```

Identify the CSS selectors in order to extract the relevant information from this page, namely

1.  job
2.  firm
3.  functional area
4.  type

Can you get all pages of ads, and not just the first one, `https://www.consultancy.uk/jobs/page/1` into a dataframe?

-   Write a function called `scrape_jobs()` that scrapes information from the webpage for consulting positions. This function should

    -   have one input: the URL of the webpage and should return a data frame with four columns (variables): job, firm, functional area, and type

    -   Test your function works with other pages too, e.g., <https://www.consultancy.uk/jobs/page/2>. Does the function seem to do what you expected it to do?

    -   Given that you have to scrape `...jobs/page/1`, `...jobs/page/2`, etc., define your URL so you can join multiple stings into one string, using `str_c()`. For instnace, if `page` is 5, what do you expect the following code to produce? *you need to remove the 1 from the base url and then append the page number. This could would look at page 15!!*

```         
base_url <- "https://www.consultancy.uk/jobs/page/1"
url <- str_c(base_url, page)
```

-   Construct a vector called `pages` that contains the numbers for each page available

-   Map the `scrape_jobs()` function over `pages` in a way that will result in a data frame called `all_consulting_jobs`.

-   Write the data frame to a csv file called `all_consulting_jobs.csv` in the `data` folder.


```r
# store as function to use again
scrape_jobs <- function(url){
  
  listings_html <- url %>%
    read_html()
  
  # store tables
  tables <-
    listings_html %>% 
      html_nodes(css = "table") %>% 
      html_table()
  
  # save table 1 to df
  table <-
    tables[[1]] %>% 
    clean_names()
  
  # definte output
  return(table)
}

# make vector for all available pages
pages = 1:8

# create urls from pages
base_url = "https://www.consultancy.uk/jobs/page/"
urls <- str_c(base_url, pages)

# map urls with scrape_jobs output
listings_all <- map_df(urls, scrape_jobs)

# write to csv
write.csv(listings_all, here::here("data", "all_consulting_jobs.csv"), row.names=FALSE)
```

# Details

-   Who did you collaborate with: n/a
-   Approximately how much time did you spend on this problem set: a few hours
-   What, if anything, gave you the most trouble: it took me a while to figure out the rvest package
