---
title: 'Homework 4'
author: "Seán O'Doherty"
date: "2023-06-07"
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



# The Bechdel Test

<https://fivethirtyeight.com/features/the-dollar-and-cents-case-against-hollywoods-exclusion-of-women/>

The [Bechdel test](https://bechdeltest.com) is a way to assess how women are depicted in Hollywood movies. In order for a movie to pass the test:

1.  It has to have at least two [named] women in it
2.  Who talk to each other
3.  About something besides a man

There is a nice article and analysis you can find here <https://fivethirtyeight.com/features/the-dollar-and-cents-case-against-hollywoods-exclusion-of-women/> We have a sample of 1394 movies and we want to fit a model to predict whether a film passes the test or not.


```r
bechdel <- read_csv(here::here("data", "bechdel.csv")) %>% 
  mutate(test = factor(test)) 
```

```
## Rows: 1394 Columns: 10
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (4): title, test, rated, genre
## dbl (6): year, budget_2013, domgross_2013, intgross_2013, metascore, imdb_ra...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
glimpse(bechdel)
```

```
## Rows: 1,394
## Columns: 10
## $ year          <dbl> 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 20…
## $ title         <chr> "12 Years a Slave", "2 Guns", "42", "47 Ronin", "A Good …
## $ test          <fct> Fail, Fail, Fail, Fail, Fail, Pass, Pass, Fail, Pass, Pa…
## $ budget_2013   <dbl> 2.00, 6.10, 4.00, 22.50, 9.20, 1.20, 1.30, 13.00, 4.00, …
## $ domgross_2013 <dbl> 5.3107035, 7.5612460, 9.5020213, 3.8362475, 6.7349198, 1…
## $ intgross_2013 <dbl> 15.8607035, 13.2493015, 9.5020213, 14.5803842, 30.424919…
## $ rated         <chr> "R", "R", "PG-13", "PG-13", "R", "R", "PG-13", "PG-13", …
## $ metascore     <dbl> 97, 55, 62, 29, 28, 55, 48, 33, 90, 58, 52, 78, 83, 53, …
## $ imdb_rating   <dbl> 8.3, 6.8, 7.6, 6.6, 5.4, 7.8, 5.7, 5.0, 7.5, 7.4, 6.2, 7…
## $ genre         <chr> "Biography", "Action", "Biography", "Action", "Action", …
```

How many films fail/pass the test, both as a number and as a %?


```r
bechdel %>% 
  
  # group by pass / fail
  group_by(test) %>% 
  
  # get counts
  summarise(film_count = n()) %>% 
  
  # calculate line count over total count
  mutate(film_pct = as.numeric(film_count) / sum(film_count))
```

```
## # A tibble: 2 × 3
##   test  film_count film_pct
##   <fct>      <int>    <dbl>
## 1 Fail         772    0.554
## 2 Pass         622    0.446
```

## Movie scores


```r
ggplot(data = bechdel, aes(
  x = metascore,
  y = imdb_rating,
  colour = test
)) +
  geom_point(alpha = .3, size = 3) +
  scale_colour_manual(values = c("tomato", "olivedrab")) +
  labs(
    x = "Metacritic score",
    y = "IMDB rating",
    colour = "Bechdel test"
  ) +
 theme_light()
```

<img src="/english/post/sod_homework4_new_files/figure-html/unnamed-chunk-3-1.png" width="672" />

# Split the data


```r
# **Split the data**

set.seed(123)

data_split <- initial_split(bechdel, # updated data
                           prop = 0.8, 
                           strata = test)

bechdel_train <- training(data_split) 
bechdel_test <- testing(data_split)
```

Check the counts and % (proportions) of the `test` variable in each set.


```r
count_and_prop <- function(df) {

  df %>% 
    
    # group by pass / fail
    group_by(test) %>% 
    
    # get counts
    summarise(film_count = n()) %>% 
    
    # calculate line count over total count
    mutate(film_pct = as.numeric(film_count) / sum(film_count))
  
}
  
count_and_prop(bechdel)
```

```
## # A tibble: 2 × 3
##   test  film_count film_pct
##   <fct>      <int>    <dbl>
## 1 Fail         772    0.554
## 2 Pass         622    0.446
```

```r
count_and_prop(bechdel_train)
```

```
## # A tibble: 2 × 3
##   test  film_count film_pct
##   <fct>      <int>    <dbl>
## 1 Fail         617    0.554
## 2 Pass         497    0.446
```

```r
count_and_prop(bechdel_test)
```

```
## # A tibble: 2 × 3
##   test  film_count film_pct
##   <fct>      <int>    <dbl>
## 1 Fail         155    0.554
## 2 Pass         125    0.446
```

## Feature exploration

## Any outliers?


```r
bechdel %>% 
  select(test, budget_2013, domgross_2013, intgross_2013, imdb_rating, metascore) %>% 

    pivot_longer(cols = 2:6,
               names_to = "feature",
               values_to = "value") %>% 
  ggplot()+
  aes(x=test, y = value, fill = test)+
  coord_flip()+
  geom_boxplot()+
  facet_wrap(~feature, scales = "free")+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x=NULL,y = NULL)
```

<img src="/english/post/sod_homework4_new_files/figure-html/unnamed-chunk-6-1.png" width="672" />

## Scatterplot - Correlation Matrix

Write a paragraph discussing the output of the following


```r
bechdel %>% 
  select(test, budget_2013, domgross_2013, intgross_2013, imdb_rating, metascore)%>% 
  ggpairs(aes(colour=test), alpha=0.2)+
  theme_bw()
```

<img src="/english/post/sod_homework4_new_files/figure-html/unnamed-chunk-7-1.png" width="672" />

*The distributions of the other variables in relation to the test result suggest that there is not a huge difference between movies which pass the test and those which fail. The small differences imply that those which fail as a whole have slightly higher budget, domestic and international gross, and imdb rating and much the same metascore than those which pass. While some of the variables exhibit correlations with each other, those correlations are observed in both the passing and failing movies.*

## Categorical variables

Write a paragraph discussing the output of the following


```r
bechdel %>% 
  group_by(genre, test) %>%
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n))
```

```
## `summarise()` has grouped output by 'genre'. You can override using the
## `.groups` argument.
```

```
## # A tibble: 24 × 4
## # Groups:   genre [14]
##    genre     test      n  prop
##    <chr>     <fct> <int> <dbl>
##  1 Action    Fail    260 0.707
##  2 Action    Pass    108 0.293
##  3 Adventure Fail     52 0.559
##  4 Adventure Pass     41 0.441
##  5 Animation Fail     63 0.677
##  6 Animation Pass     30 0.323
##  7 Biography Fail     36 0.554
##  8 Biography Pass     29 0.446
##  9 Comedy    Fail    138 0.427
## 10 Comedy    Pass    185 0.573
## # ℹ 14 more rows
```

```r
bechdel %>% 
  group_by(rated, test) %>%
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n))
```

```
## `summarise()` has grouped output by 'rated'. You can override using the
## `.groups` argument.
```

```
## # A tibble: 10 × 4
## # Groups:   rated [5]
##    rated test      n  prop
##    <chr> <fct> <int> <dbl>
##  1 G     Fail     16 0.615
##  2 G     Pass     10 0.385
##  3 NC-17 Fail      5 0.833
##  4 NC-17 Pass      1 0.167
##  5 PG    Fail    115 0.561
##  6 PG    Pass     90 0.439
##  7 PG-13 Fail    283 0.529
##  8 PG-13 Pass    252 0.471
##  9 R     Fail    353 0.568
## 10 R     Pass    269 0.432
```

*The distripbution of passes and fails does vary quite a bit across genres. Musical and Thiller all pass. Documentary and Sci-Fi all fail. Across the genres which have the most films, Horror and Comedy have the highest proportion of passes. Action has the most films and a low proportion of passes.*

*The rating does not appear to affect the proportion of passes much with the exception of NC-17 which passes much less frequently than the others.*

# Train first models. `test ~ metascore + imdb_rating`


```r
lr_mod <- logistic_reg() %>% 
  set_engine(engine = "glm") %>% 
  set_mode("classification")

lr_mod
```

```
## Logistic Regression Model Specification (classification)
## 
## Computational engine: glm
```

```r
tree_mod <- decision_tree() %>% 
  set_engine(engine = "C5.0") %>% 
  set_mode("classification")

tree_mod 
```

```
## Decision Tree Model Specification (classification)
## 
## Computational engine: C5.0
```


```r
lr_fit <- lr_mod %>% # parsnip model
  fit(test ~ metascore + imdb_rating, # a formula
    data = bechdel_train # dataframe
  )

tree_fit <- tree_mod %>% # parsnip model
  fit(test ~ metascore + imdb_rating, # a formula
    data = bechdel_train # dataframe
  )
```

## Logistic regression


```r
lr_fit %>%
  broom::tidy()
```

```
## # A tibble: 3 × 5
##   term        estimate std.error statistic  p.value
##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)   2.80     0.494        5.68 1.35e- 8
## 2 metascore     0.0207   0.00536      3.86 1.13e- 4
## 3 imdb_rating  -0.625    0.100       -6.24 4.36e-10
```

```r
lr_preds <- lr_fit %>%
  augment(new_data = bechdel_train) %>%
  mutate(.pred_match = if_else(test == .pred_class, 1, 0))
```

### Confusion matrix


```r
lr_preds %>% 
  conf_mat(truth = test, estimate = .pred_class) %>% 
  autoplot(type = "heatmap")
```

<img src="/english/post/sod_homework4_new_files/figure-html/unnamed-chunk-12-1.png" width="672" />

## Decision Tree


```r
tree_preds <- tree_fit %>%
  augment(new_data = bechdel) %>%
  mutate(.pred_match = if_else(test == .pred_class, 1, 0)) 
```


```r
tree_preds %>% 
  conf_mat(truth = test, estimate = .pred_class) %>% 
  autoplot(type = "heatmap")
```

<img src="/english/post/sod_homework4_new_files/figure-html/unnamed-chunk-14-1.png" width="672" />

## Draw the decision tree


```r
draw_tree <- 
    rpart::rpart(
        test ~ metascore + imdb_rating,
        data = bechdel_train, # uses data that contains both birth weight and `low`
        control = rpart::rpart.control(maxdepth = 5, cp = 0, minsplit = 10)
    ) %>% 
    partykit::as.party()
plot(draw_tree)
```

<img src="/english/post/sod_homework4_new_files/figure-html/unnamed-chunk-15-1.png" width="672" />

# Cross Validation

Run the code below. What does it return?

*It splits the data into 10 'folds'. The splits column conrtains list objects which store the tibbles of each fold. I have adapted the code to only 5 folds as 10 was crashing my computer.*


```r
set.seed(123)
bechdel_folds <- vfold_cv(data = bechdel_train, 
                          v = 5, 
                          strata = test)
bechdel_folds
```

```
## #  5-fold cross-validation using stratification 
## # A tibble: 5 × 2
##   splits            id   
##   <list>            <chr>
## 1 <split [890/224]> Fold1
## 2 <split [890/224]> Fold2
## 3 <split [892/222]> Fold3
## 4 <split [892/222]> Fold4
## 5 <split [892/222]> Fold5
```

## `fit_resamples()`

Trains and tests a resampled model.


```r
lr_fit <- lr_mod %>%
  fit_resamples(
    test ~ metascore + imdb_rating,
    resamples = bechdel_folds
  )


tree_fit <- tree_mod %>%
  fit_resamples(
    test ~ metascore + imdb_rating,
    resamples = bechdel_folds
  )
```

## `collect_metrics()`

Unnest the metrics column from a tidymodels `fit_resamples()`


```r
collect_metrics(lr_fit)
```

```
## # A tibble: 2 × 6
##   .metric  .estimator  mean     n std_err .config             
##   <chr>    <chr>      <dbl> <int>   <dbl> <chr>               
## 1 accuracy binary     0.576     5  0.0169 Preprocessor1_Model1
## 2 roc_auc  binary     0.604     5  0.0136 Preprocessor1_Model1
```

```r
collect_metrics(tree_fit)
```

```
## # A tibble: 2 × 6
##   .metric  .estimator  mean     n std_err .config             
##   <chr>    <chr>      <dbl> <int>   <dbl> <chr>               
## 1 accuracy binary     0.551     5 0.00765 Preprocessor1_Model1
## 2 roc_auc  binary     0.527     5 0.0119  Preprocessor1_Model1
```


```r
tree_preds <- tree_mod %>% 
  fit_resamples(
    test ~ metascore + imdb_rating, 
    resamples = bechdel_folds,
    control = control_resamples(save_pred = TRUE) #<<
  )

# What does the data for ROC look like?
tree_preds %>% 
  collect_predictions() %>% 
  roc_curve(truth = test, .pred_Fail)  
```

```
## # A tibble: 15 × 3
##    .threshold specificity sensitivity
##         <dbl>       <dbl>       <dbl>
##  1   -Inf          0           1     
##  2      0.277      0           1     
##  3      0.316      0.0201      0.976 
##  4      0.441      0.0302      0.968 
##  5      0.450      0.0885      0.916 
##  6      0.514      0.145       0.878 
##  7      0.549      0.262       0.771 
##  8      0.554      0.451       0.598 
##  9      0.558      0.650       0.399 
## 10      0.569      0.783       0.256 
## 11      0.673      0.901       0.136 
## 12      0.778      0.966       0.0665
## 13      0.781      0.988       0.0389
## 14      0.782      0.990       0.0194
## 15    Inf          1           0
```

```r
# Draw the ROC
tree_preds %>% 
  collect_predictions() %>% 
  roc_curve(truth = test, .pred_Fail) %>% 
  autoplot()
```

<img src="/english/post/sod_homework4_new_files/figure-html/unnamed-chunk-19-1.png" width="672" />

# Build a better training set with `recipes`

## Preprocessing options

-   Encode categorical predictors
-   Center and scale variables
-   Handle class imbalance
-   Impute missing data
-   Perform dimensionality reduction
-   ... ...

## To build a recipe

1.  Start the `recipe()`
2.  Define the variables involved
3.  Describe **prep**rocessing [step-by-step]

## Collapse Some Categorical Levels

Do we have any `genre` with few observations? Assign genres that have less than 3% to a new category 'Other'

<img src="/english/post/sod_homework4_new_files/figure-html/unnamed-chunk-20-1.png" width="672" />


```r
movie_rec <-
  recipe(test ~ .,
         data = bechdel_train) %>%
  
  # Genres with less than 3% will be in a catewgory 'Other'
    step_other(genre, threshold = .03) 
```

## Before recipe


```r
bechdel_train %>% 
  count(genre, sort = TRUE)
```

```
## # A tibble: 14 × 2
##    genre           n
##    <chr>       <int>
##  1 Action        293
##  2 Comedy        254
##  3 Drama         213
##  4 Adventure      75
##  5 Animation      72
##  6 Crime          68
##  7 Horror         68
##  8 Biography      50
##  9 Mystery         7
## 10 Fantasy         5
## 11 Sci-Fi          3
## 12 Thriller        3
## 13 Documentary     2
## 14 Musical         1
```

## After recipe


```r
movie_rec %>% 
  prep() %>% 
  bake(new_data = bechdel_train) %>% 
  count(genre, sort = TRUE)
```

```
## # A tibble: 9 × 2
##   genre         n
##   <fct>     <int>
## 1 Action      293
## 2 Comedy      254
## 3 Drama       213
## 4 Adventure    75
## 5 Animation    72
## 6 Crime        68
## 7 Horror       68
## 8 Biography    50
## 9 other        21
```

## `step_dummy()`

Converts nominal data into numeric dummy variables


```r
movie_rec <- recipe(test ~ ., data = bechdel) %>%
  step_other(genre, threshold = .03) %>% 
  step_dummy(all_nominal_predictors()) 

movie_rec 
```

```
## 
```

```
## ── Recipe ──────────────────────────────────────────────────────────────────────
```

```
## 
```

```
## ── Inputs
```

```
## Number of variables by role
```

```
## outcome:   1
## predictor: 9
```

```
## 
```

```
## ── Operations
```

```
## • Collapsing factor levels for: genre
```

```
## • Dummy variables from: all_nominal_predictors()
```

## Let's think about the modelling

What if there were no films with `rated` NC-17 in the training data?

-   Will the model have a coefficient for `rated` NC-17?
-   What will happen if the test data includes a film with `rated` NC-17?

## `step_novel()`

Adds a catch-all level to a factor for any new values not encountered in model training, which lets R intelligently predict new levels in the test set.


```r
movie_rec <- recipe(test ~ ., data = bechdel) %>%
  step_other(genre, threshold = .03) %>% 
  step_novel(all_nominal_predictors) %>% # Use *before* `step_dummy()` so new level is dummified
  step_dummy(all_nominal_predictors()) 
```

## `step_zv()`

Intelligently handles zero variance variables (variables that contain only a single value)


```r
movie_rec <- recipe(test ~ ., data = bechdel) %>%
  step_other(genre, threshold = .03) %>% 
  step_novel(all_nominal(), -all_outcomes()) %>% # Use *before* `step_dummy()` so new level is dummified
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_numeric(), -all_outcomes()) 
```

## `step_normalize()`

Centers then scales numeric variable (mean = 0, sd = 1)


```r
movie_rec <- recipe(test ~ ., data = bechdel) %>%
  step_other(genre, threshold = .03) %>% 
  step_novel(all_nominal(), -all_outcomes()) %>% # Use *before* `step_dummy()` so new level is dummified
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_numeric(), -all_outcomes())  %>% 
  step_normalize(all_numeric()) 
```

## `step_corr()`

Removes highly correlated variables


```r
movie_rec <- recipe(test ~ ., data = bechdel) %>%
  step_other(genre, threshold = .03) %>% 
  step_novel(all_nominal(), -all_outcomes()) %>% # Use *before* `step_dummy()` so new level is dummified
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_numeric(), -all_outcomes())  %>% 
  step_normalize(all_numeric()) %>% 
  step_corr(all_predictors(), threshold = 0.75, method = "spearman") 



movie_rec
```

```
## 
```

```
## ── Recipe ──────────────────────────────────────────────────────────────────────
```

```
## 
```

```
## ── Inputs
```

```
## Number of variables by role
```

```
## outcome:   1
## predictor: 9
```

```
## 
```

```
## ── Operations
```

```
## • Collapsing factor levels for: genre
```

```
## • Novel factor level assignment for: all_nominal(), -all_outcomes()
```

```
## • Dummy variables from: all_nominal(), -all_outcomes()
```

```
## • Zero variance filter on: all_numeric(), -all_outcomes()
```

```
## • Centering and scaling for: all_numeric()
```

```
## • Correlation filter on: all_predictors()
```

# Define different models to fit


```r
## Model Building

# 1. Pick a `model type`
# 2. set the `engine`
# 3. Set the `mode`: regression or classification

# Logistic regression
log_spec <-  logistic_reg() %>%  # model type
  set_engine(engine = "glm") %>%  # model engine
  set_mode("classification") # model mode

# Show your model specification
log_spec
```

```
## Logistic Regression Model Specification (classification)
## 
## Computational engine: glm
```

```r
# Decision Tree
tree_spec <- decision_tree() %>%
  set_engine(engine = "C5.0") %>%
  set_mode("classification")

tree_spec
```

```
## Decision Tree Model Specification (classification)
## 
## Computational engine: C5.0
```

```r
# Random Forest
library(ranger)

rf_spec <- 
  rand_forest() %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")


# Boosted tree (XGBoost)
library(xgboost)
```

```
## 
## Attaching package: 'xgboost'
```

```
## The following object is masked from 'package:dplyr':
## 
##     slice
```

```r
xgb_spec <- 
  boost_tree() %>% 
  set_engine("xgboost") %>% 
  set_mode("classification") 

# K-nearest neighbour (k-NN)
knn_spec <- 
  nearest_neighbor(neighbors = 4) %>% # we can adjust the number of neighbors 
  set_engine("kknn") %>% 
  set_mode("classification") 
```

# Bundle recipe and model with `workflows`


```r
log_wflow <- # new workflow object
 workflow() %>% # use workflow function
 add_recipe(movie_rec) %>%   # use the new recipe
 add_model(log_spec)   # add your model spec

# show object
log_wflow
```

```
## ══ Workflow ════════════════════════════════════════════════════════════════════
## Preprocessor: Recipe
## Model: logistic_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## 6 Recipe Steps
## 
## • step_other()
## • step_novel()
## • step_dummy()
## • step_zv()
## • step_normalize()
## • step_corr()
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Logistic Regression Model Specification (classification)
## 
## Computational engine: glm
```

```r
## A few more workflows

tree_wflow <-
 workflow() %>%
 add_recipe(movie_rec) %>% 
 add_model(tree_spec) 

rf_wflow <-
 workflow() %>%
 add_recipe(movie_rec) %>% 
 add_model(rf_spec) 

xgb_wflow <-
 workflow() %>%
 add_recipe(movie_rec) %>% 
 add_model(xgb_spec)

knn_wflow <-
 workflow() %>%
 add_recipe(movie_rec) %>% 
 add_model(knn_spec)
```

HEADS UP

1.  How many models have you specified?

    *There are 5 different models specified in different workflows.*

2.  What's the difference between a model specification and a workflow?

    *The model is the the mathematical component and the workflow is combining that with the recipe which is the data handling elements.*

3.  Do you need to add a formula (e.g., `test ~ .`) if you have a recipe?

    *This is contained in the recipe.*

# Model Comparison

You now have all your models. Adapt the code from slides `code-from-slides-CA-housing.R`, line 400 onwards to assess which model gives you the best classification.


```r
## Logistic regression results{.smaller}

log_res <- log_wflow %>% 
  fit_resamples(
    resamples = bechdel_folds, 
    metrics = metric_set(
      recall, precision, f_meas, accuracy,
      kap, roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)) 
```

```
## → A | warning: glm.fit: algorithm did not converge
```

```
## There were issues with some computations   A: x1
```

```
## → B | warning: prediction from rank-deficient fit; attr(*, "non-estim") has doubtful cases
```

```
## There were issues with some computations   A: x1
There were issues with some computations   A: x1   B: x1
## There were issues with some computations   A: x2   B: x1
## There were issues with some computations   A: x2   B: x2
## There were issues with some computations   A: x3   B: x2
## There were issues with some computations   A: x3   B: x3
## There were issues with some computations   A: x4   B: x3
## There were issues with some computations   A: x4   B: x4
## There were issues with some computations   A: x5   B: x4
## There were issues with some computations   A: x5   B: x5
## There were issues with some computations   A: x5   B: x5
```

```r
# Show average performance over all folds (note that we use log_res):
log_res %>%  collect_metrics(summarize = TRUE)
```

```
## # A tibble: 8 × 6
##   .metric   .estimator    mean     n std_err .config             
##   <chr>     <chr>        <dbl> <int>   <dbl> <chr>               
## 1 accuracy  binary      0.481      5  0.0191 Preprocessor1_Model1
## 2 f_meas    binary      0.461      5  0.0254 Preprocessor1_Model1
## 3 kap       binary     -0.0185     5  0.0379 Preprocessor1_Model1
## 4 precision binary      0.544      5  0.0247 Preprocessor1_Model1
## 5 recall    binary      0.404      5  0.0325 Preprocessor1_Model1
## 6 roc_auc   binary      0.475      5  0.0243 Preprocessor1_Model1
## 7 sens      binary      0.404      5  0.0325 Preprocessor1_Model1
## 8 spec      binary      0.578      5  0.0404 Preprocessor1_Model1
```

```r
# Show performance for every single fold:
log_res %>%  collect_metrics(summarize = FALSE)
```

```
## # A tibble: 40 × 5
##    id    .metric   .estimator .estimate .config             
##    <chr> <chr>     <chr>          <dbl> <chr>               
##  1 Fold1 recall    binary        0.5    Preprocessor1_Model1
##  2 Fold1 precision binary        0.590  Preprocessor1_Model1
##  3 Fold1 f_meas    binary        0.541  Preprocessor1_Model1
##  4 Fold1 accuracy  binary        0.531  Preprocessor1_Model1
##  5 Fold1 kap       binary        0.0687 Preprocessor1_Model1
##  6 Fold1 sens      binary        0.5    Preprocessor1_Model1
##  7 Fold1 spec      binary        0.57   Preprocessor1_Model1
##  8 Fold1 roc_auc   binary        0.533  Preprocessor1_Model1
##  9 Fold2 recall    binary        0.347  Preprocessor1_Model1
## 10 Fold2 precision binary        0.478  Preprocessor1_Model1
## # ℹ 30 more rows
```


```r
## `collect_predictions()` and get confusion matrix{.smaller}

log_pred <- log_res %>% collect_predictions()

log_pred %>%  conf_mat(test, .pred_class) 
```

```
##           Truth
## Prediction Fail Pass
##       Fail  249  210
##       Pass  368  287
```

```r
log_pred %>% 
  conf_mat(test, .pred_class) %>% 
  autoplot(type = "heatmap")
```

<img src="/english/post/sod_homework4_new_files/figure-html/unnamed-chunk-32-1.png" width="672" />

```r
## ROC Curve

log_pred %>% 
  group_by(id) %>% # id contains our folds
  roc_curve(test, .pred_Fail) %>% 
  autoplot()
```

<img src="/english/post/sod_homework4_new_files/figure-html/unnamed-chunk-32-2.png" width="672" />


```r
## Decision Tree results

tree_res <-
  tree_wflow %>% 
  fit_resamples(
    resamples = bechdel_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
    ) 

tree_res %>%  collect_metrics(summarize = TRUE)
```

```
## # A tibble: 8 × 6
##   .metric   .estimator  mean     n std_err .config             
##   <chr>     <chr>      <dbl> <int>   <dbl> <chr>               
## 1 accuracy  binary     0.604     5 0.0146  Preprocessor1_Model1
## 2 f_meas    binary     0.645     5 0.00941 Preprocessor1_Model1
## 3 kap       binary     0.197     5 0.0319  Preprocessor1_Model1
## 4 precision binary     0.642     5 0.0176  Preprocessor1_Model1
## 5 recall    binary     0.650     5 0.0100  Preprocessor1_Model1
## 6 roc_auc   binary     0.586     5 0.0208  Preprocessor1_Model1
## 7 sens      binary     0.650     5 0.0100  Preprocessor1_Model1
## 8 spec      binary     0.547     5 0.0341  Preprocessor1_Model1
```


```r
## Random Forest

rf_res <-
  rf_wflow %>% 
  fit_resamples(
    resamples = bechdel_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
    ) 

rf_res %>%  collect_metrics(summarize = TRUE)
```

```
## # A tibble: 8 × 6
##   .metric   .estimator  mean     n std_err .config             
##   <chr>     <chr>      <dbl> <int>   <dbl> <chr>               
## 1 accuracy  binary     0.635     5  0.0239 Preprocessor1_Model1
## 2 f_meas    binary     0.699     5  0.0191 Preprocessor1_Model1
## 3 kap       binary     0.245     5  0.0505 Preprocessor1_Model1
## 4 precision binary     0.645     5  0.0205 Preprocessor1_Model1
## 5 recall    binary     0.763     5  0.0226 Preprocessor1_Model1
## 6 roc_auc   binary     0.663     5  0.0204 Preprocessor1_Model1
## 7 sens      binary     0.763     5  0.0226 Preprocessor1_Model1
## 8 spec      binary     0.477     5  0.0389 Preprocessor1_Model1
```


```r
## Boosted tree - XGBoost

xgb_res <- 
  xgb_wflow %>% 
  fit_resamples(
    resamples = bechdel_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
    ) 

xgb_res %>% collect_metrics(summarize = TRUE)
```

```
## # A tibble: 8 × 6
##   .metric   .estimator  mean     n std_err .config             
##   <chr>     <chr>      <dbl> <int>   <dbl> <chr>               
## 1 accuracy  binary     0.609     5 0.00944 Preprocessor1_Model1
## 2 f_meas    binary     0.660     5 0.0102  Preprocessor1_Model1
## 3 kap       binary     0.202     5 0.0201  Preprocessor1_Model1
## 4 precision binary     0.638     5 0.00971 Preprocessor1_Model1
## 5 recall    binary     0.686     5 0.0204  Preprocessor1_Model1
## 6 roc_auc   binary     0.630     5 0.0151  Preprocessor1_Model1
## 7 sens      binary     0.686     5 0.0204  Preprocessor1_Model1
## 8 spec      binary     0.515     5 0.0276  Preprocessor1_Model1
```


```r
## K-nearest neighbour

knn_res <- 
  knn_wflow %>% 
  fit_resamples(
    resamples = bechdel_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
    ) 
```

```
## → A | warning: While computing binary `precision()`, no predicted events were detected (i.e. `true_positive + false_positive = 0`). 
##                Precision is undefined in this case, and `NA` will be returned.
##                Note that 123 true event(s) actually occured for the problematic event level, 'Fail'.
```

```
## There were issues with some computations   A: x1
## There were issues with some computations   A: x1
```

```
## 
```

```r
knn_res %>% collect_metrics(summarize = TRUE)
```

```
## # A tibble: 8 × 6
##   .metric   .estimator     mean     n std_err .config             
##   <chr>     <chr>         <dbl> <int>   <dbl> <chr>               
## 1 accuracy  binary     0.532        5 0.0217  Preprocessor1_Model1
## 2 f_meas    binary     0.712        4 0.00167 Preprocessor1_Model1
## 3 kap       binary     0.000864     5 0.00450 Preprocessor1_Model1
## 4 precision binary     0.554        4 0.00131 Preprocessor1_Model1
## 5 recall    binary     0.797        5 0.199   Preprocessor1_Model1
## 6 roc_auc   binary     0.549        5 0.0289  Preprocessor1_Model1
## 7 sens      binary     0.797        5 0.199   Preprocessor1_Model1
## 8 spec      binary     0.204        5 0.199   Preprocessor1_Model1
```


```r
## Model Comparison

log_metrics <- 
  log_res %>% 
  collect_metrics(summarise = TRUE) %>%
  # add the name of the model to every row
  mutate(model = "Logistic Regression") 

tree_metrics <- 
  tree_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "Decision Tree")

rf_metrics <- 
  rf_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "Random Forest")

xgb_metrics <- 
  xgb_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "XGBoost")

knn_metrics <- 
  knn_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "Knn")
```


```r
# create dataframe with all models
model_compare <- bind_rows(log_metrics,
                           tree_metrics,
                           rf_metrics,
                           xgb_metrics,
                           knn_metrics) 

#Pivot wider to create barplot
  model_comp <- model_compare %>% 
  select(model, .metric, mean, std_err) %>% 
  pivot_wider(names_from = .metric, values_from = c(mean, std_err)) 
```


```r
# show mean are under the curve (ROC-AUC) for every model
model_comp %>% 
  arrange(mean_roc_auc) %>% 
  mutate(model = fct_reorder(model, mean_roc_auc)) %>% # order results
  ggplot(aes(model, mean_roc_auc, fill=model)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
   geom_text(
     size = 3,
     aes(label = round(mean_roc_auc, 2), 
         y = mean_roc_auc + 0.08),
     vjust = 1
  )+
  theme_light()+
  theme(legend.position = "none")+
  labs(y = NULL)
```

<img src="/english/post/sod_homework4_new_files/figure-html/unnamed-chunk-39-1.png" width="672" />


```r
## `last_fit()` on test set

# - `last_fit()`  fits a model to the whole training data and evaluates it on the test set. 
# - provide the workflow object of the best model as well as the data split object (not the training data). 
 
last_fit_rf <- last_fit(rf_wflow, 
                        split = data_split,
                        metrics = metric_set(
                          accuracy, f_meas, kap, precision,
                          recall, roc_auc, sens, spec))

last_fit_rf %>% collect_metrics(summarize = TRUE)
```

```
## # A tibble: 8 × 4
##   .metric   .estimator .estimate .config             
##   <chr>     <chr>          <dbl> <chr>               
## 1 accuracy  binary         0.579 Preprocessor1_Model1
## 2 f_meas    binary         0.663 Preprocessor1_Model1
## 3 kap       binary         0.120 Preprocessor1_Model1
## 4 precision binary         0.595 Preprocessor1_Model1
## 5 recall    binary         0.748 Preprocessor1_Model1
## 6 sens      binary         0.748 Preprocessor1_Model1
## 7 spec      binary         0.368 Preprocessor1_Model1
## 8 roc_auc   binary         0.624 Preprocessor1_Model1
```

```r
#Compare to training
rf_res %>% collect_metrics(summarize = TRUE)
```

```
## # A tibble: 8 × 6
##   .metric   .estimator  mean     n std_err .config             
##   <chr>     <chr>      <dbl> <int>   <dbl> <chr>               
## 1 accuracy  binary     0.635     5  0.0239 Preprocessor1_Model1
## 2 f_meas    binary     0.699     5  0.0191 Preprocessor1_Model1
## 3 kap       binary     0.245     5  0.0505 Preprocessor1_Model1
## 4 precision binary     0.645     5  0.0205 Preprocessor1_Model1
## 5 recall    binary     0.763     5  0.0226 Preprocessor1_Model1
## 6 roc_auc   binary     0.663     5  0.0204 Preprocessor1_Model1
## 7 sens      binary     0.763     5  0.0226 Preprocessor1_Model1
## 8 spec      binary     0.477     5  0.0389 Preprocessor1_Model1
```


```r
## Variable importance using `{vip}` package

library(vip)

last_fit_rf %>% 
  pluck(".workflow", 1) %>%   
  pull_workflow_fit() %>% 
  vip(num_features = 10) +
  theme_light()
```

```
## Warning: `pull_workflow_fit()` was deprecated in workflows 0.2.3.
## ℹ Please use `extract_fit_parsnip()` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

<img src="/english/post/sod_homework4_new_files/figure-html/unnamed-chunk-41-1.png" width="672" />


```r
## Final Confusion Matrix

last_fit_rf %>%
  collect_predictions() %>% 
  conf_mat(test, .pred_class) %>% 
  autoplot(type = "heatmap")
```

<img src="/english/post/sod_homework4_new_files/figure-html/unnamed-chunk-42-1.png" width="672" />


```r
## Final ROC curve
last_fit_rf %>% 
  collect_predictions() %>% 
  roc_curve(test, .pred_Fail) %>% 
  autoplot()
```

<img src="/english/post/sod_homework4_new_files/figure-html/unnamed-chunk-43-1.png" width="672" />

# Deliverables

There is a lot of explanatory text, comments, etc. You do not need these, so delete them and produce a stand-alone document that you could share with someone. Knit the edited and completed R Markdown (Rmd) file as a Word or HTML document (use the "Knit" button at the top of the script editor window) and upload it to Canvas. You must be commiting and pushing your changes to your own Github repo as you go along.

# Details

-   Who did you collaborate with: TYPE NAMES HERE
-   Approximately how much time did you spend on this problem set: ANSWER HERE
-   What, if anything, gave you the most trouble: ANSWER HERE

**Please seek out help when you need it,** and remember the [15-minute rule](https://dsb2023.netlify.app/syllabus/#the-15-minute-rule){target="_blank"}. You know enough R (and have enough examples of code from class and your readings) to be able to do this. If you get stuck, ask for help from others, post a question on Slack-- and remember that I am here to help too!

> As a true test to yourself, do you understand the code you submitted and are you able to explain it to someone else?

# Rubric

13/13: Problem set is 100% completed. Every question was attempted and answered, and most answers are correct. Code is well-documented (both self-documented and with additional comments as necessary). Used tidyverse, instead of base R. Graphs and tables are properly labelled. Analysis is clear and easy to follow, either because graphs are labeled clearly or you've written additional text to describe how you interpret the output. Multiple Github commits. Work is exceptional. I will not assign these often.

8/13: Problem set is 60--80% complete and most answers are correct. This is the expected level of performance. Solid effort. Hits all the elements. No clear mistakes. Easy to follow (both the code and the output). A few Github commits.

5/13: Problem set is less than 60% complete and/or most answers are incorrect. This indicates that you need to improve next time. I will hopefully not assign these often. Displays minimal effort. Doesn't complete all components. Code is poorly written and not documented. Uses the same type of plot for each graph, or doesn't use plots appropriate for the variables being analyzed. No Github commits.
