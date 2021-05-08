Propensity Score Weighting
================

Propensity score weighting is one of the techniques used in controlling
for selection biases in nonexperimental studies. Propensity scores can
be used as weights to account for selection assignment differences
between treatment and comparison groups. One of the advantages of this
approach is that all the individuals in the study can be used for the
outcomes evaluation.

In order to conduct an analysis involving propensity scores, the authors
follow a very specific set of steps that include:

1.  Outcome analysis without the use of propensity scores

2.  Balance analysis prior to the implementation of propensity scores

3.  Propensity score estimation

4.  Weight estimation using propensity scores

5.  Balance analysis after implementing propensity scores

6.  Outcomes analysis using propensity scores in a weighted regression

We will use the “lalonde” datafile that is part of the {MatchIt} pckage.
The dataset was developed by Lalonde (1986) to demonstrate the impact of
a retraining program (National Supported Work Demonstration). The data
file included in {MatchIt} contains 614 observations, with 185 in the
treatment group and 429 in the control group. The outcome variable is
re78, which is the income for individuals in both groups during 1978.

#### 0. Read in the data

``` r
attach(lalonde)
lalonde <- lalonde %>% 
  mutate(black = ifelse(race == "black", 1, 0),
         hispan = ifelse(race == "hispan", 1, 0))
```

#### 1. Outcome analysis without the use of propensity scores

In this step, we run an outcome analysis without the use of propensity
scores.

This analysis is helpful to gauge what might have been the result of the
outcome analysis had we not used propensity scores to control for
potential selection biases associated with group assignment.

``` r
model0 <- lm(re78 ~ treat + black + hispan + married, data = lalonde)
summary(model0)
```


    Call:
    lm(formula = re78 ~ treat + black + hispan + married, data = lalonde)

    Residuals:
       Min     1Q Median     3Q    Max 
     -9804  -5648  -2109   3945  54660 

    Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
    (Intercept)   6292.9      566.7  11.104  < 2e-16 ***
    treat         1294.1      820.5   1.577 0.115266    
    black        -1939.4      806.9  -2.404 0.016531 *  
    hispan        -369.4      971.4  -0.380 0.703877    
    married       2217.4      644.0   3.443 0.000614 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 7360 on 609 degrees of freedom
    Multiple R-squared:  0.03579,   Adjusted R-squared:  0.02945 
    F-statistic: 5.651 on 4 and 609 DF,  p-value: 0.0001803

From the results shown above, there is no treatment effect (since the
variable treat is not statistically significant), but the `black` and
`married` covariates are significant at p &lt; 0.05

#### 2. Balance analysis prior to the implementation of propensity scores

This step is intended to assess the degree of bias between the groups
before the propensity score is incorporated in the analysis.

Typical analyses include statistical comparisons between the covariate
(as DV) and the treatment variable (as IV).

``` r
yvars <- names(lalonde)[!names(lalonde) %in% c("treat", "race")]
xvar <- "treat"

for(i in 1: length(yvars)){
form <- as.formula(paste(yvars[i], xvar, sep = "~"))
print(form)
print(t.test(form, data = lalonde))
  
}
```

    age ~ treat

        Welch Two Sample t-test

    data:  age by treat
    t = 2.9911, df = 510.57, p-value = 0.002914
    alternative hypothesis: true difference in means is not equal to 0
    95 percent confidence interval:
     0.7598127 3.6683610
    sample estimates:
    mean in group 0 mean in group 1 
           28.03030        25.81622 

    educ ~ treat

        Welch Two Sample t-test

    data:  educ by treat
    t = -0.54676, df = 485.37, p-value = 0.5848
    alternative hypothesis: true difference in means is not equal to 0
    95 percent confidence interval:
     -0.5076687  0.2866393
    sample estimates:
    mean in group 0 mean in group 1 
           10.23543        10.34595 

    married ~ treat

        Welch Two Sample t-test

    data:  married by treat
    t = 8.5961, df = 439.29, p-value < 2.2e-16
    alternative hypothesis: true difference in means is not equal to 0
    95 percent confidence interval:
     0.2496379 0.3976248
    sample estimates:
    mean in group 0 mean in group 1 
          0.5128205       0.1891892 

    nodegree ~ treat

        Welch Two Sample t-test

    data:  nodegree by treat
    t = -2.7127, df = 374.01, p-value = 0.006982
    alternative hypothesis: true difference in means is not equal to 0
    95 percent confidence interval:
     -0.19210040 -0.03064263
    sample estimates:
    mean in group 0 mean in group 1 
          0.5967366       0.7081081 

    re74 ~ treat

        Welch Two Sample t-test

    data:  re74 by treat
    t = 7.2456, df = 475.99, p-value = 1.748e-12
    alternative hypothesis: true difference in means is not equal to 0
    95 percent confidence interval:
     2568.067 4479.258
    sample estimates:
    mean in group 0 mean in group 1 
           5619.237        2095.574 

    re75 ~ treat

        Welch Two Sample t-test

    data:  re75 by treat
    t = 3.2776, df = 356.22, p-value = 0.00115
    alternative hypothesis: true difference in means is not equal to 0
    95 percent confidence interval:
      373.742 1495.116
    sample estimates:
    mean in group 0 mean in group 1 
           2466.484        1532.055 

    re78 ~ treat

        Welch Two Sample t-test

    data:  re78 by treat
    t = 0.93773, df = 326.41, p-value = 0.3491
    alternative hypothesis: true difference in means is not equal to 0
    95 percent confidence interval:
     -697.192 1967.244
    sample estimates:
    mean in group 0 mean in group 1 
           6984.170        6349.144 

    black ~ treat

        Welch Two Sample t-test

    data:  black by treat
    t = -19.344, df = 382.86, p-value < 2.2e-16
    alternative hypothesis: true difference in means is not equal to 0
    95 percent confidence interval:
     -0.7055419 -0.5753502
    sample estimates:
    mean in group 0 mean in group 1 
          0.2027972       0.8432432 

    hispan ~ treat

        Welch Two Sample t-test

    data:  hispan by treat
    t = 3.4091, df = 501.34, p-value = 0.0007042
    alternative hypothesis: true difference in means is not equal to 0
    95 percent confidence interval:
     0.03505288 0.13041048
    sample estimates:
    mean in group 0 mean in group 1 
         0.14219114      0.05945946 

The results show us that there is imbalance in treatment assignment in
all the variables expect `married`.

#### 3. Propensity score estimation

The next step is the estimation of the propensity scores that will be
used as weights in the analysis.

Logistic regression is used to determine the probability of membership
in the treatment or control group, given the specific set of selection
variables included.

``` r
## Fit the logistic regression model
ps <- glm(treat ~ age + educ + nodegree + re74 + re75, 
          family = binomial(), data = lalonde)

## Extract the predicted values (propensity scores) and attach them to the data
lalonde2 <- augment_columns(ps,lalonde,type.predict = "response") %>%
            rename(propensity = .fitted)
```

#### 4. Weight estimation using propensity scores

Calculate the inverse probability weights by using the formula:

$$\\frac{Treatment}{Propensity} + \\frac{1-Treatment}{1-Propensity}$$

This gives more weight to the misclassified observations and less weight
to the classifications that are classified correctly.

``` r
lalonde2 <- lalonde2 %>% 
  mutate(ipw = (treat / propensity) + ((1 - treat)/(1 - propensity)))
```

#### 5. Balance analysis after implementing propensity scores

The ultimate purpose of using propensity scores is to balance the
treatment/comparison groups on the observed covariates.

To assess the success of the propensity scores as weights in a weighted
regression for removing selection bias, a new set of tests to check the
balance should be performed.

However, now weighted linear regressions are performed using computed
propensity scores as weights.

``` r
yvars <- names(lalonde)[!names(lalonde) %in% c("treat", "race")]
xvar <- "treat"

for(i in 1: length(yvars)){
form <- as.formula(paste(yvars[i], xvar, sep = "~"))
print(form)
print(summary(lm(form, weight = (ipw),  data = lalonde2)))
  
}
```

    age ~ treat

    Call:
    lm(formula = form, data = lalonde2, weights = (ipw))

    Weighted Residuals:
        Min      1Q  Median      3Q     Max 
    -17.451  -9.841  -2.753   7.752  44.090 

    Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
    (Intercept)  27.2532     0.5114  53.287   <2e-16 ***
    treat        -1.3053     0.7005  -1.863   0.0629 .  
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 12.65 on 612 degrees of freedom
    Multiple R-squared:  0.005641,  Adjusted R-squared:  0.004016 
    F-statistic: 3.472 on 1 and 612 DF,  p-value: 0.06291

    educ ~ treat

    Call:
    lm(formula = form, data = lalonde2, weights = (ipw))

    Weighted Residuals:
         Min       1Q   Median       3Q      Max 
    -20.4391  -2.5297   0.2682   1.9830  29.3906 

    Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
    (Intercept)  10.2948     0.1505  68.414   <2e-16 ***
    treat         0.5178     0.2061   2.512   0.0123 *  
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 3.721 on 612 degrees of freedom
    Multiple R-squared:  0.01021,   Adjusted R-squared:  0.008589 
    F-statistic: 6.311 on 1 and 612 DF,  p-value: 0.01226

    married ~ treat

    Call:
    lm(formula = form, data = lalonde2, weights = (ipw))

    Weighted Residuals:
        Min      1Q  Median      3Q     Max 
    -1.5214 -0.5724 -0.4581  0.5798  6.4610 

    Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
    (Intercept)  0.47256    0.02826  16.725  < 2e-16 ***
    treat       -0.17324    0.03870  -4.476 9.06e-06 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.6988 on 612 degrees of freedom
    Multiple R-squared:  0.0317,    Adjusted R-squared:  0.03012 
    F-statistic: 20.04 on 1 and 612 DF,  p-value: 9.06e-06

    nodegree ~ treat

    Call:
    lm(formula = form, data = lalonde2, weights = (ipw))

    Weighted Residuals:
        Min      1Q  Median      3Q     Max 
    -5.0300 -0.6936  0.4319  0.5140  2.3102 

    Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
    (Intercept)  0.62518    0.02907  21.504   <2e-16 ***
    treat       -0.07969    0.03982  -2.001   0.0458 *  
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.719 on 612 degrees of freedom
    Multiple R-squared:  0.006501,  Adjusted R-squared:  0.004878 
    F-statistic: 4.005 on 1 and 612 DF,  p-value: 0.04582

    re74 ~ treat

    Call:
    lm(formula = form, data = lalonde2, weights = (ipw))

    Weighted Residuals:
       Min     1Q Median     3Q    Max 
    -17352  -7758  -5149   3391 244477 

    Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
    (Intercept)   4616.8      574.7   8.034 4.88e-15 ***
    treat         3910.1      787.2   4.967 8.82e-07 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 14210 on 612 degrees of freedom
    Multiple R-squared:  0.03875,   Adjusted R-squared:  0.03718 
    F-statistic: 24.67 on 1 and 612 DF,  p-value: 8.821e-07

    re75 ~ treat

    Call:
    lm(formula = form, data = lalonde2, weights = (ipw))

    Weighted Residuals:
       Min     1Q Median     3Q    Max 
     -7133  -2958  -2241   1060  74057 

    Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
    (Intercept)   2208.5      239.6   9.219  < 2e-16 ***
    treat         1296.7      328.1   3.952 8.67e-05 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 5925 on 612 degrees of freedom
    Multiple R-squared:  0.02488,   Adjusted R-squared:  0.02329 
    F-statistic: 15.62 on 1 and 612 DF,  p-value: 8.667e-05

    re78 ~ treat

    Call:
    lm(formula = form, data = lalonde2, weights = (ipw))

    Weighted Residuals:
       Min     1Q Median     3Q    Max 
    -45605  -7958  -3979   4273 248982 

    Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
    (Intercept)   6631.9      590.3  11.234  < 2e-16 ***
    treat         3013.3      808.6   3.726 0.000212 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 14600 on 612 degrees of freedom
    Multiple R-squared:  0.02219,   Adjusted R-squared:  0.02059 
    F-statistic: 13.89 on 1 and 612 DF,  p-value: 0.0002122

    black ~ treat

    Call:
    lm(formula = form, data = lalonde2, weights = (ipw))

    Weighted Residuals:
        Min      1Q  Median      3Q     Max 
    -2.3815 -0.2621 -0.2286  0.2174  1.2142 

    Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
    (Intercept)  0.21524    0.02212   9.731   <2e-16 ***
    treat        0.65308    0.03030  21.556   <2e-16 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.547 on 612 degrees of freedom
    Multiple R-squared:  0.4316,    Adjusted R-squared:  0.4307 
    F-statistic: 464.7 on 1 and 612 DF,  p-value: < 2.2e-16

    hispan ~ treat

    Call:
    lm(formula = form, data = lalonde2, weights = (ipw))

    Weighted Residuals:
         Min       1Q   Median       3Q      Max 
    -0.48813 -0.17206 -0.15140 -0.08341  2.59743 

    Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
    (Intercept)  0.14129    0.01708   8.272 8.24e-16 ***
    treat       -0.08835    0.02340  -3.776 0.000175 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.4224 on 612 degrees of freedom
    Multiple R-squared:  0.02277,   Adjusted R-squared:  0.02118 
    F-statistic: 14.26 on 1 and 612 DF,  p-value: 0.0001746

#### 6. Outcomes analysis using propensity scores in a weighted regression

The final step in the analysis is to run the outcomes model using the
propensity scores as weights. In this example the outcomes model
includes re78 as the outcome variable and treat, black and married as
independent variables.

``` r
model3 <- lm(re78 ~ treat + black + hispan + married, data = lalonde2,
             weights = (ipw))
summary(model3)
```


    Call:
    lm(formula = re78 ~ treat + black + hispan + married, data = lalonde2, 
        weights = (ipw))

    Weighted Residuals:
       Min     1Q Median     3Q    Max 
    -36753 -10082  -2749   4968 195767 

    Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
    (Intercept)   2645.8      769.6   3.438 0.000626 ***
    treat         3684.0     1003.8   3.670 0.000264 ***
    black         1068.6     1076.7   0.992 0.321372    
    hispan        -232.6     1389.7  -0.167 0.867133    
    married       8018.0      787.4  10.183  < 2e-16 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 13520 on 609 degrees of freedom
    Multiple R-squared:  0.1655,    Adjusted R-squared:   0.16 
    F-statistic:  30.2 on 4 and 609 DF,  p-value: < 2.2e-16

The results above show that the effect of the treatment was
statistically significant after the groups are balanced using propensity
scores.
