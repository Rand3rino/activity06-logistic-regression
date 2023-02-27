Activity 6 - Logistic Regression
================

## Libraries

``` r
library(tidyverse)
library(tidymodels)
library(GGally)
```

## The Data

``` r
resume <- readr::read_csv("https://www.openintro.org/data/csv/resume.csv")
```

The variable of interest is `received_callback`, which is a binary
variable.

``` r
resume %>% 
  ggplot(aes(x=received_callback==1)) + 
  geom_bar() + 
  theme_bw() + 
  ylab("Count") + 
  xlab("Received Call Back") + 
  ggtitle("# of Received call Back")
```

![](activity06_files/figure-gfm/target%20visualization-1.png)<!-- -->

``` r
# TODO Add Percentages
resume %>% 
  group_by(received_callback) %>% 
  summarise(n()) %>% 
  knitr::kable()
```

| received\_callback |  n() |
|-------------------:|-----:|
|                  0 | 4478 |
|                  1 |  392 |

The probability of someone getting a call back from their resume is
8.05% or 392/(4478+392). The odds of someone getting a call back is
8.75% or 392/4478.

## Logistic Regression

``` r
resume %>% 
  group_by(received_callback, race) %>% 
  summarise(n()) %>% 
  knitr::kable()
```

    ## `summarise()` has grouped output by 'received_callback'. You can override using
    ## the `.groups` argument.

| received\_callback | race  |  n() |
|-------------------:|:------|-----:|
|                  0 | black | 2278 |
|                  0 | white | 2200 |
|                  1 | black |  157 |
|                  1 | white |  235 |

The probability that a randomly selected résumé/person perceived as
Black will be called back is 6.45%. The odds that a randomly selected
résumé/person perceived as Black will be called back is 6.89%.

### Logistic Model

``` r
# The {tidymodels} method for logistic regression requires that the response be a factor variable
resume <- resume %>% 
  mutate(received_callback = as.factor(received_callback))

resume_mod <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(received_callback ~ race, data = resume, family = "binomial")

tidy(resume_mod) %>% 
  knitr::kable(digits = 3)
```

| term        | estimate | std.error | statistic | p.value |
|:------------|---------:|----------:|----------:|--------:|
| (Intercept) |   -2.675 |     0.083 |   -32.417 |       0 |
| racewhite   |    0.438 |     0.107 |     4.083 |       0 |

The regression equation corresponding to résumés/persons perceived as
White is `received_callback = 0.438(racewhite) - 2.675`.

The *simplified* estimated regression equation corresponding to
résumés/persons perceived as Black is \`received\_callback = ????
(raceblack) + 2.675\`\`

## Multinomial Logistic Regression

Select records in Chicago and select the relevant variables. Rename
`gender` to `sex` and converts `race` to capital letters and `sex` to
full words.

``` r
resume_select <- resume %>% 
  rename(sex = gender) %>% 
  filter(job_city == "Chicago") %>% 
  mutate(race = case_when(
         race == "white" ~ "White",
         TRUE ~ "Black"
       ),
       sex = case_when(
         sex == "f" ~ "female",
         TRUE ~ "male"
       )) %>% 
  select(received_callback, years_experience, race, sex)
```

## Relationship Exploration

There is a visible difference between callbacks for `race` and `sex`.

``` r
resume_select %>% 
  ggbivariate(outcome = "received_callback") + 
  theme_bw()
```

![](activity06_files/figure-gfm/relationship%20exploration-1.png)<!-- -->

## Fitting the Model

``` r
mult_log_mod <- glm(received_callback ~ years_experience + race + sex, data = resume_select, family = "binomial")

tidy(mult_log_mod) %>% 
  knitr::kable(digits = 3)
```

| term              | estimate | std.error | statistic | p.value |
|:------------------|---------:|----------:|----------:|--------:|
| (Intercept)       |   -3.279 |     0.181 |   -18.082 |   0.000 |
| years\_experience |    0.045 |     0.016 |     2.751 |   0.006 |
| raceWhite         |    0.426 |     0.157 |     2.720 |   0.007 |
| sexmale           |    0.580 |     0.203 |     2.857 |   0.004 |

``` r
tidy(mult_log_mod, exponentiate = TRUE) %>% 
  knitr::kable(digits = 3)
```

| term              | estimate | std.error | statistic | p.value |
|:------------------|---------:|----------:|----------:|--------:|
| (Intercept)       |    0.038 |     0.181 |   -18.082 |   0.000 |
| years\_experience |    1.046 |     0.016 |     2.751 |   0.006 |
| raceWhite         |    1.532 |     0.157 |     2.720 |   0.007 |
| sexmale           |    1.786 |     0.203 |     2.857 |   0.004 |
