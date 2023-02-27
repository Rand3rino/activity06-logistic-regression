Day 2 - Multiple Logistic Regression
================

In this repository/directory you should see one items:

- `README.md` - this document.

You will continue working in your
`day01-qualitative-explanatory/activity03.Rmd` file that you started on
[Day 1](../day01-qualitative-explanatory).

## Task 1: Pull your changes from GitHub into RStudio

You successfully updated your GitHub repo from the main
[`README`](../README), but we still need to update your RStudio files.
Read these directions first, then work through them.

- In the **Git** pane of RStudio (upper right-hand area), locate and
  click on the
  <img src="../README-img/pull-icon.png" alt="knit" width = "20"/>
  **Pull** icon.
- After a few moments (you might need to provide your GitHub username
  and PAT), your **Files** pane will be updated with the new items.

Since you are likely looking at the `README` (this page) on GitHub,
nothing of importance for your work today was brought in. However, it is
always a good habit to **pull** from GitHub before starting to work
after taking a break. For example, if you were collaborating on a
project with others, they might do work at different times than you (or
even at the same time). It is always good to solve problems on your end
before **push**ing to GitHub and causing more problems.

## Today

We will explore the following question: Is there a difference in call
back rates in Chicago jobs, after adjusting for the an applicant’s years
of experience, years of college, race, and sex? Specifically, we will
fit the following model, where $\hat{p}$ is the estimated probability of
receiving a callback for a job in Chicago.

$$
\begin{equation*}
\log\left(\frac{\hat{p}}{1-\hat{p}}\right) = \hat\beta_0 + \hat\beta_1 \times (\texttt{years\\_experience}) + \hat\beta_2 \times (\texttt{race:White}) + \hat\beta_3 \times (\texttt{sex:male})
\end{equation*}
$$

Note that the researchers have the variable labeled `gender`, but this
is likely meant to be `sex`. [Curious as to what the difference
is](https://www.merriam-webster.com/words-at-play/sex-vs-gender-how-they2019re-different)?

- Create a new R code chunk and type the following, then run your code
  chunk or knit your document.

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

After doing this, answer the following question:

1.  Explain what six things the above code does in the context of this
    problem.

## Task 2: Relationship Exploration

There are many variables in this model. Let’s explore each explanatory
variable’s relationship with the response variable. Note that I tried to
explore this using `GGally::ggbivariate`, but kept running into an error
that I did not have time to explore.

- Create a new R code chunk and create an appropriate data visualization
  to explore the relationship between `resume_select` and each of the
  explanatory variables, then run your code chunk or knit your document.

After doing this, answer the following question:

2.  Describe any patterns. What do you notice?

## Task 3: Fitting the model

Aside: I kept running into an issue using `{tidymodels}` to fit this
model so I defaulted back to a method that I know works using `glm`. I
will keep exploring why I was experiencing issues and update you all
with a more modern method later this semester.

- Create a new R code chunk and type the following, then run your code
  chunk or knit your document.

  ``` r
  mult_log_mod <- glm(received_callback ~ years_experience + race + sex, data = resume_select, family = "binomial")

  tidy(mult_log_mod)
  ```

Focusing on the estimated coefficient for `years_experience`, we would
say:

> For each additional year of experience for an applicant in Chicago, we
> expect the *log odds* of an applicant receiving a call back to
> increase by 0.045 units. Assuming applicants have similar time in
> spent in college, similar inferred races, and similar inferred sex.

This interpretation is somewhat confusing because we are describing this
in *log odds*. Fortunately, we can convert these back to odds using the
following transformation:

$$
\text{odds} = e^{\log(\text{odds})}
$$

- Create a new R code chunk and type the following, then run your code
  chunk or knit your document.

  ``` r
  tidy(mult_log_mod, exponentiate = TRUE) %>% 
    knitr::kable(digits = 3)
  ```

After doing this, answer the following question:

2.  Interpret the estimated coefficient for `years_experience`.

## Task 4: Assessing model fit

Now we want to check the residuals of this model to check the model’s
fit. As we saw for multiple linear regression, there are various kinds
of residuals that try to adjust for various features of the data. Two
new residuals to explore are *Pearson residuals* and *Deviance
residuals*.

**Pearson residuals**

The Pearson residual corrects for the unequal variance in the raw
residuals by dividing by the standard deviation.

$$
\text{Pearson}_i = \frac{y_i - \hat{p}_i}{\sqrt{\hat{p}_i(1 - \hat{p}_i)}}
$$

**Deviance residuals**

Deviance residuals are popular because the sum of squares of these
residuals is the deviance statistic. We will talk more about this later
in the semester.

$$
d_i = \text{sign}(y_i - \hat{p}_i)\sqrt{2\Big[y_i\log\Big(\frac{y_i}{\hat{p}_i}\Big) + (1 - y_i)\log\Big(\frac{1 - y_i}{1 - \hat{p}_i}\Big)\Big]}
$$

Since Pearson residuals are similar to residuals that we have already
explored, we will instead focus on the deviance residuals.

- Create a new R code chunk and type the following, then run your code
  chunk or knit your document.

  ``` r
  # To store residuals and create row number variable
  mult_log_aug <- augment(mult_log_mod, type.predict = "response", 
                        type.residuals = "deviance") %>% 
                        mutate(id = row_number())

  # Plot residuals vs fitted values
  ggplot(data = mult_log_aug, aes(x = .fitted, y = .resid)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "red") + 
  labs(x = "Fitted values", 
       y = "Deviance residuals", 
       title = "Deviance residuals vs. fitted")

  # Plot residuals vs row number
  ggplot(data = mult_log_aug, aes(x = id, y = .resid)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "red") + 
  labs(x = "id", 
       y = "Deviance residuals", 
       title = "Deviance residuals vs. id")
  ```

Here we produced two residual plots: the deviance residuals against the
fitted values and the deviance variables against the index id (an index
plot). The index plot allows us to easily see some of the more extreme
observations - there are a lot ($|d_i| > 2$ is quiet alarming). The
residual plot may look odd (why are there two distinct lines?!?), but
this is a pretty typical shape when working with a binary response
variable (the original data is really either a 0 or a 1). In general
because there are so many extreme values in the index plot, this model
leaves room for improvement.

## What is next?

We will continue exploring classification models with *linear
discriminant analysis* (LDA). This is a form of dimension reduction
(like principle component analysis or factor analysis) to find a linear
combination explanatory variables to hopeful reduce the overall number
of explanatory variables used in our final model.
