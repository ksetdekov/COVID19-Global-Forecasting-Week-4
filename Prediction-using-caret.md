---
title: "Covid-19 prediction"
author: "Kirill Setdekov"
date: "12 04 2020"
output:
  html_document:
    keep_md: yes
---



Load packages

```r
library(caret)
library(dplyr)
library(readr)
```

Load data

```r
enriched <-
        read_csv("input/covid19-enriched-dataset-week-2/enriched_covid_19_week_2.csv")
```

```
## Parsed with column specification:
## cols(
##   .default = col_double(),
##   Country_Region = col_character(),
##   Province_State = col_character(),
##   Date = col_date(format = "")
## )
```

```
## See spec(...) for full column specifications.
```

```r
head(enriched[, 1:10])
```

```
## # A tibble: 6 x 10
##      Id Country_Region Province_State Date       ConfirmedCases Fatalities
##   <dbl> <chr>          <chr>          <date>              <dbl>      <dbl>
## 1     1 Afghanistan    <NA>           2020-01-22              0          0
## 2     2 Afghanistan    <NA>           2020-01-23              0          0
## 3     3 Afghanistan    <NA>           2020-01-24              0          0
## 4     4 Afghanistan    <NA>           2020-01-25              0          0
## 5     5 Afghanistan    <NA>           2020-01-26              0          0
## 6     6 Afghanistan    <NA>           2020-01-27              0          0
## # ... with 4 more variables: `age_0-4` <dbl>, `age_5-9` <dbl>,
## #   `age_10-14` <dbl>, `age_15-19` <dbl>
```

```r
test <-
        read_csv("input/covid19-global-forecasting-week-4/test.csv",
                 col_types = cols(Date = col_date(format = "%Y-%m-%d")))
submission <-
        read_csv("input/covid19-global-forecasting-week-4/submission.csv")
```

```
## Parsed with column specification:
## cols(
##   ForecastId = col_double(),
##   ConfirmedCases = col_double(),
##   Fatalities = col_double()
## )
```

```r
train <-
        read_csv("input/covid19-global-forecasting-week-4/train.csv")
```

```
## Parsed with column specification:
## cols(
##   Id = col_double(),
##   Province_State = col_character(),
##   Country_Region = col_character(),
##   Date = col_date(format = ""),
##   ConfirmedCases = col_double(),
##   Fatalities = col_double()
## )
```

```r
head(test)
```

```
## # A tibble: 6 x 4
##   ForecastId Province_State Country_Region Date      
##        <dbl> <chr>          <chr>          <date>    
## 1          1 <NA>           Afghanistan    2020-04-02
## 2          2 <NA>           Afghanistan    2020-04-03
## 3          3 <NA>           Afghanistan    2020-04-04
## 4          4 <NA>           Afghanistan    2020-04-05
## 5          5 <NA>           Afghanistan    2020-04-06
## 6          6 <NA>           Afghanistan    2020-04-07
```

```r
head(train)
```

```
## # A tibble: 6 x 6
##      Id Province_State Country_Region Date       ConfirmedCases Fatalities
##   <dbl> <chr>          <chr>          <date>              <dbl>      <dbl>
## 1     1 <NA>           Afghanistan    2020-01-22              0          0
## 2     2 <NA>           Afghanistan    2020-01-23              0          0
## 3     3 <NA>           Afghanistan    2020-01-24              0          0
## 4     4 <NA>           Afghanistan    2020-01-25              0          0
## 5     5 <NA>           Afghanistan    2020-01-26              0          0
## 6     6 <NA>           Afghanistan    2020-01-27              0          0
```
take a look at Russia

```r
enriched %>% filter(Country_Region == "Russia") %>% head()
```

```
## # A tibble: 6 x 38
##      Id Country_Region Province_State Date       ConfirmedCases Fatalities
##   <dbl> <chr>          <chr>          <date>              <dbl>      <dbl>
## 1 19401 Russia         <NA>           2020-01-22              0          0
## 2 19402 Russia         <NA>           2020-01-23              0          0
## 3 19403 Russia         <NA>           2020-01-24              0          0
## 4 19404 Russia         <NA>           2020-01-25              0          0
## 5 19405 Russia         <NA>           2020-01-26              0          0
## 6 19406 Russia         <NA>           2020-01-27              0          0
## # ... with 32 more variables: `age_0-4` <dbl>, `age_5-9` <dbl>,
## #   `age_10-14` <dbl>, `age_15-19` <dbl>, `age_20-24` <dbl>, `age_25-29` <dbl>,
## #   `age_30-34` <dbl>, `age_35-39` <dbl>, `age_40-44` <dbl>, `age_45-49` <dbl>,
## #   `age_50-54` <dbl>, `age_55-59` <dbl>, `age_60-64` <dbl>, `age_65-69` <dbl>,
## #   `age_70-74` <dbl>, `age_75-79` <dbl>, `age_80-84` <dbl>, `age_85-89` <dbl>,
## #   `age_90-94` <dbl>, `age_95-99` <dbl>, `age_100+` <dbl>, total_pop <dbl>,
## #   smokers_perc <dbl>, density <dbl>, urbanpop <dbl>, hospibed <dbl>,
## #   lung <dbl>, femalelung <dbl>, malelung <dbl>, restrictions <dbl>,
## #   quarantine <dbl>, schools <dbl>
```

```r
enriched_countries <- enriched$Country_Region %>% unique()
train_countries <- train$Country_Region %>% unique() 
train_countries_df <- data.frame(train_countries)
train_countries_df <- train_countries_df %>% mutate(isinenriched = ifelse(train_countries %in% enriched_countries,1,0))
```
Data is constant over time. We can leftjoin in to the train data and replicate the last row

Need data transformation:
4 countries are more detailed in enriched data
* Australia - all values aggreaged over regions, starting with Australia
* Canada - all values aggreaged over regions, starting with Canada
* China - all values aggreaged over regions, starting with China
* US - all values aggreaged over regions, starting with US


```r
australia_names <- enriched_countries[9:16]
enriched$Country_Region[enriched$Country_Region %in% australia_names] <- "Australia"

canada_names <- enriched_countries[37:46]
enriched$Country_Region[enriched$Country_Region %in% canada_names] <- "Canada"


china_names <- enriched_countries[50:82]
enriched$Country_Region[enriched$Country_Region %in% china_names] <- "China"


us_names <- enriched_countries[225:278]
enriched$Country_Region[enriched$Country_Region %in% us_names] <- "US"


uk_names <- enriched_countries[282:287]
enriched$Country_Region[enriched$Country_Region %in% uk_names] <- "United Kingdom"

enriched$Country_Region[enriched$Country_Region %in% enriched_countries[109:117]] <- "France"

enriched$Country_Region[enriched$Country_Region %in% enriched_countries[92:93]] <- "Denmark"
enriched$Country_Region[enriched$Country_Region %in% enriched_countries[174:176]] <- "Netherlands"


# what dates we have?
enriched %>% group_by(Country_Region, Province_State) %>% select(Date) %>% summarise(max = max(Date), min = min(Date))
```

```
## # A tibble: 294 x 4
## # Groups:   Country_Region [173]
##    Country_Region      Province_State               max        min       
##    <chr>               <chr>                        <date>     <date>    
##  1 Afghanistan         <NA>                         2020-03-25 2020-01-22
##  2 Albania             <NA>                         2020-03-25 2020-01-22
##  3 Algeria             <NA>                         2020-03-25 2020-01-22
##  4 Andorra             <NA>                         2020-03-25 2020-01-22
##  5 Angola              <NA>                         2020-03-25 2020-01-22
##  6 Antigua and Barbuda <NA>                         2020-03-25 2020-01-22
##  7 Argentina           <NA>                         2020-03-25 2020-01-22
##  8 Armenia             <NA>                         2020-03-25 2020-01-22
##  9 Australia           Australian Capital Territory 2020-03-25 2020-01-22
## 10 Australia           New South Wales              2020-03-25 2020-01-22
## # ... with 284 more rows
```

```r
train %>% group_by(Country_Region, Province_State) %>% select(Date) %>% summarise(max = max(Date), min = min(Date))
```

```
## # A tibble: 313 x 4
## # Groups:   Country_Region [184]
##    Country_Region      Province_State               max        min       
##    <chr>               <chr>                        <date>     <date>    
##  1 Afghanistan         <NA>                         2020-04-11 2020-01-22
##  2 Albania             <NA>                         2020-04-11 2020-01-22
##  3 Algeria             <NA>                         2020-04-11 2020-01-22
##  4 Andorra             <NA>                         2020-04-11 2020-01-22
##  5 Angola              <NA>                         2020-04-11 2020-01-22
##  6 Antigua and Barbuda <NA>                         2020-04-11 2020-01-22
##  7 Argentina           <NA>                         2020-04-11 2020-01-22
##  8 Armenia             <NA>                         2020-04-11 2020-01-22
##  9 Australia           Australian Capital Territory 2020-04-11 2020-01-22
## 10 Australia           New South Wales              2020-04-11 2020-01-22
## # ... with 303 more rows
```

```r
test %>% group_by(Country_Region, Province_State) %>% select(Date) %>% summarise(max = max(Date), min = min(Date))
```

```
## # A tibble: 313 x 4
## # Groups:   Country_Region [184]
##    Country_Region      Province_State               max        min       
##    <chr>               <chr>                        <date>     <date>    
##  1 Afghanistan         <NA>                         2020-05-14 2020-04-02
##  2 Albania             <NA>                         2020-05-14 2020-04-02
##  3 Algeria             <NA>                         2020-05-14 2020-04-02
##  4 Andorra             <NA>                         2020-05-14 2020-04-02
##  5 Angola              <NA>                         2020-05-14 2020-04-02
##  6 Antigua and Barbuda <NA>                         2020-05-14 2020-04-02
##  7 Argentina           <NA>                         2020-05-14 2020-04-02
##  8 Armenia             <NA>                         2020-05-14 2020-04-02
##  9 Australia           Australian Capital Territory 2020-05-14 2020-04-02
## 10 Australia           New South Wales              2020-05-14 2020-04-02
## # ... with 303 more rows
```

```r
enriched <- enriched %>% select(-c("Id", "ConfirmedCases", "Fatalities"))
enriched_last_date <- enriched %>% filter(Date =="2020-03-25")
enriched_last_date <- enriched_last_date %>% select(-c("Date"))

rm(train_countries, train_countries_df, australia_names, canada_names, china_names, enriched_countries, train_countries, uk_names, us_names)
```

# join enriched data to train and test

```r
enrich <- function(df, enriched, enriched_last_date) {
        require(dplyr)
  p1 <- left_join(df, enriched, by = c("Province_State", "Country_Region", "Date"))
  p2 <- left_join(df, enriched_last_date, by = c("Province_State", "Country_Region"))
  p1 <- p1 %>% filter(Date <= "2020-03-25")
  p2 <- p2 %>% filter(Date > "2020-03-25")
  df <- bind_rows(p1,p2)
  rm(p1,p2)
  return(df)
}
train <- enrich(train, enriched, enriched_last_date)
test <- enrich(test, enriched, enriched_last_date)
```


11 are missing, will immute with knn

* Botswana
* Burma
* Burundi
* Kosovo
* MS Zaandam	
* Malawi
* Sao Tome and Principe
* Sierra Leone
* South Sudan
* West Bank and Gaza
* Western Sahara


