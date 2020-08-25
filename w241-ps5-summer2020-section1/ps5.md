Problem Set 5
================
Field Experiments

1. Online advertising natural experiment.
=========================================

These are simulated data (closely, although not entirely) based on a real example, adopted from Randall Lewis’ dissertation at MIT.

Problem Setup
-------------

Imagine Yahoo! sells homepage ads to advertisers that are quasi-randomly assigned by whether the user loads the Yahoo! homepage (www.yahoo.com) on an even or odd second of the day. More specifically, the setup is as follows. On any given week, Monday through Sunday, two ad campaigns are running on Yahoo!’s homepage. If a user goes to www.yahoo.com during an even second that week (e.g., Monday at 12:30:58pm), the ads for the advertiser are shown. But if the user goes to www.yahoo.com during an odd second during that week (e.g., Monday at 12:30:59), the ads for other products are shown. (If a user logs onto Yahoo! once on an even second and once on an odd second, they are shown the first of the campaigns the first time and the second of the campaigns the second time. Assignment is not persistent within users.)

This natural experiment allows us to use the users who log onto Yahoo! during odd seconds/the ad impressions from odd seconds as a randomized control group for users who log onto Yahoo! during even seconds/the ad impressions from even seconds. (We will assume throughout the problem there is no effect of viewing advertiser 2’s ads, from odd seconds, on purchases for advertiser 1, the product advertised on even seconds.)

Imagine you are an advertiser who has purchased advertising from Yahoo! that is subject to this randomization on two occasions. Here is a link to (fake) data on 500,000 randomly selected users who visited Yahoo!’s homepage during each of your two advertising campaigns, one you conducted for product A in March and one you conducted for product B in August (~250,000 users for each of the two experiments). Each row in the dataset corresponds to a user exposed to one of these campaigns.

``` r
library(data.table)
library(stargazer)
library(dplyr)

library(sandwich)
library(lmtest)
```

``` r
d <- fread('./data/ps5_no1.csv')
head(d)
```

    ##    product_b total_ad_exposures_week1 treatment_ad_exposures_week1 week0
    ## 1:         1                        4                            3   5.5
    ## 2:         1                        1                            1   6.2
    ## 3:         1                        3                            1   0.0
    ## 4:         0                        5                            0   0.0
    ## 5:         0                        1                            1   7.6
    ## 6:         1                        4                            4   6.3
    ##    week1 week2 week3 week4 week5 week6 week7 week8 week9 week10
    ## 1:   6.2   0.0   0.0   0.0   0.0   0.0     0   9.7   4.1    0.0
    ## 2:   0.0   8.6   2.4   0.0   7.4   0.0     0   0.0   5.7    0.0
    ## 3:   5.3   0.0   8.1   7.8   3.3   0.0     0   9.4   0.0    0.0
    ## 4:   4.1   0.0   8.8   5.8   5.9   0.0     0   0.0   9.6    0.0
    ## 5:   3.6   4.6   5.5   7.2   7.1   0.0     0   0.0   0.0    0.0
    ## 6:   5.5   9.8   5.0   0.0   0.0   7.7     0  11.0   4.8    6.9

The variables in the dataset are described below:

-   **product\_b**: an indicator for whether the data is from your campaign for product A (in which case it is set to 0), sold beginning on March 1, or for product B, sold beginning on August 1 (in which case it is set to 1). That is, there are two experiments in this dataset, and this variable tells you which experiment the data belong to.
-   **treatment\_ad\_exposures\_week1**: number of ad exposures for the product being advertised during the campaign. (One can also think of this variable as “number of times each user visited Yahoo! homepage on an even second during the week of the campaign.”)
-   **total\_ad\_exposures\_week1**: number of ad exposures on the Yahoo! homepage each user had during the ad campaign, which is the sum of exposures to the “treatment ads” for the product being advertised (delivered on even seconds) and exposures to the “control ads” for unrelated products (delivered on odd seconds). (One can also think of this variable as “total number of times each user visited the Yahoo! homepage during the week of the campaign.”)
-   **week0**: For the treatment product, the revenues from each user in the week prior to the launch of the advertising campaign.
-   **week1**: For the treatment product, the revenues from each user in the week during the advertising campaign. The ad campaign ends on the last day of week 1.
-   **week2-week10**: Revenue from each user for the treatment product sold in the weeks subsequent to the campaign. The ad campaign was not active during this time.

Simplifying assumptions you should make when answering this problem:

-   The effect of treatment ad exposures on purchases is linear. That is, the first exposure has the same effect as the second exposure.
-   There is no effect of being exposed to the odd-second ads on purchases for the product being advertised on the even second.
-   Every Yahoo! user visits the Yahoo! home page at most six times a week.
-   You can assume that treatment ad exposures do not cause changes in future ad exposures. That is, assume that getting a treatment ad at 9:00am doesn’t cause you to be more (or less) likely to visit the Yahoo home pages on an even second that afternoon, or on subsequent days.

Questions to Answer
-------------------

1.  Run a crosstab (`table`) of `total_ad_exposures_week1` and `treatment_ad_exposures_week1` to sanity check that the distribution of impressions looks as it should. Does it seem reasonable? Why does it look like this? (No computation required here, just a brief verbal response.)

    ``` r
    d[, table(total_ad_exposures_week1,treatment_ad_exposures_week1)]
    ```

        ##                         treatment_ad_exposures_week1
        ## total_ad_exposures_week1     0     1     2     3     4     5     6
        ##                        0 61182     0     0     0     0     0     0
        ##                        1 36754 37215     0     0     0     0     0
        ##                        2 21143 42036 20965     0     0     0     0
        ##                        3 10683 32073 32314 10726     0     0     0
        ##                        4  5044 20003 30432 20223  5115     0     0
        ##                        5  2045 10563 20970 20793 10293  2131     0
        ##                        6   729  4437 10977 14771 11147  4486   750

    **The distribution makes sense to me and verifies that people the `total_ad_exposures_week1` variable is indeed the total number of ads since `treatment_ad_exposures_week1` never has a higher value in any observation (that is no one see the treatment ad more times than the total number of ads. This is indicated by all the zeros in the table). Also, we can see that the distribution looks like a binomial distribution which makes sense. Since it's practically random whether people will see the treatment ads vs control ads, like a flip of a coin, it's more likely that poeple see a mix of different ads than only the control ads or the treatment ads when viewing multiple ads (this is reflected in the rows, where the counts are higher towards the middle of each row).**

2.  Your colleague proposes the code printed below to analyze this experiment: `lm(week1 ~ treatment_ad_exposures_week1, data)` You are suspicious. Run a placebo test with the prior week’s purchases as the outcome and report the results. Did the placebo test “succeed” or “fail”? Why do you say so?

    ``` r
    mod_b <- d[, lm(week0 ~ treatment_ad_exposures_week1)]
    summary(mod_b)
    ```

        ## 
        ## Call:
        ## lm(formula = week0 ~ treatment_ad_exposures_week1)
        ## 
        ## Residuals:
        ##    Min     1Q Median     3Q    Max 
        ## -3.248 -2.196 -1.670  2.430  8.330 
        ## 
        ## Coefficients:
        ##                              Estimate Std. Error t value Pr(>|t|)    
        ## (Intercept)                  1.669685   0.006027   277.0   <2e-16 ***
        ## treatment_ad_exposures_week1 0.263099   0.003155    83.4   <2e-16 ***
        ## ---
        ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
        ## 
        ## Residual standard error: 2.796 on 499998 degrees of freedom
        ## Multiple R-squared:  0.01372,    Adjusted R-squared:  0.01372 
        ## F-statistic:  6955 on 1 and 499998 DF,  p-value: < 2.2e-16

    **I think the placebo failed because the result shows the treatment is associated with pre-treatment purchase behavior with high significance.**

3.  The placebo test suggests that there is something wrong with our experiment or our data analysis. We suggest looking for a problem with the data analysis. Do you see something that might be spoiling the randomness of the treatment variable? How can you improve your analysis to get rid of this problem? Why does the placebo test turn out the way it does? What one thing needs to be done to analyze the data correctly? Please provide a brief explanation of why, not just what needs to be done. **There might be an issue where people who are on yahoo seeking things are just more likely to purchase the treatment product even without seeing the ad. Or maybe the campaign just have a better ability to track users purchase when they go through yahoo. Therefore, this causes a correlation between purchasing recorded and traffic on yahoo website, and when treatment ad exposure is the only variable on the right-hand side, it also takes the "credit" of the traffic factor which it shouldn't. Therefore, in order to address this issue, we can control for the variable `total_ad_exposures_week1` to control for subjects' "tendency to use yahoo" and see whether after that we still see an association between exposure to treatment ad and pre-treatment purchasing behavior.**

4.  Implement the procedure you propose from part (c), run the placebo test for the Week 0 data again, and report the results. (This placebo test should pass; if it does not, re-evaluate your strategy before wasting time proceeding.)

    ``` r
    # control for tendency to visit yahoo as a factor so we don't assume linearity
    mod_d <- d[, lm(week0 ~ treatment_ad_exposures_week1 +
                            as.factor(total_ad_exposures_week1))]
    summary(mod_d)
    ```

        ## 
        ## Call:
        ## lm(formula = week0 ~ treatment_ad_exposures_week1 + as.factor(total_ad_exposures_week1))
        ## 
        ## Residuals:
        ##    Min     1Q Median     3Q    Max 
        ## -2.847 -2.084 -1.616  2.458  7.818 
        ## 
        ## Coefficients:
        ##                                       Estimate Std. Error t value Pr(>|t|)
        ## (Intercept)                           1.306783   0.011234 116.328   <2e-16
        ## treatment_ad_exposures_week1         -0.002287   0.004629  -0.494    0.621
        ## as.factor(total_ad_exposures_week1)1  0.311564   0.015362  20.281   <2e-16
        ## as.factor(total_ad_exposures_week1)2  0.551112   0.015469  35.627   <2e-16
        ## as.factor(total_ad_exposures_week1)3  0.779854   0.016264  47.948   <2e-16
        ## as.factor(total_ad_exposures_week1)4  1.003116   0.017545  57.174   <2e-16
        ## as.factor(total_ad_exposures_week1)5  1.236092   0.019372  63.810   <2e-16
        ## as.factor(total_ad_exposures_week1)6  1.539865   0.021981  70.053   <2e-16
        ##                                         
        ## (Intercept)                          ***
        ## treatment_ad_exposures_week1            
        ## as.factor(total_ad_exposures_week1)1 ***
        ## as.factor(total_ad_exposures_week1)2 ***
        ## as.factor(total_ad_exposures_week1)3 ***
        ## as.factor(total_ad_exposures_week1)4 ***
        ## as.factor(total_ad_exposures_week1)5 ***
        ## as.factor(total_ad_exposures_week1)6 ***
        ## ---
        ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
        ## 
        ## Residual standard error: 2.779 on 499992 degrees of freedom
        ## Multiple R-squared:  0.02563,    Adjusted R-squared:  0.02562 
        ## F-statistic:  1879 on 7 and 499992 DF,  p-value: < 2.2e-16

    **The control works. Now there is no significant association between treatment variable and pre-treatment purchasing.**

5.  Now estimate the causal effect of each ad exposure on purchases during the week of the campaign itself using the same technique that passed the placebo test in part (d).

    ``` r
    mod_e <- d[, lm(week1 ~ treatment_ad_exposures_week1 + 
                            as.factor(total_ad_exposures_week1))]
    summary(mod_e)
    ```

        ## 
        ## Call:
        ## lm(formula = week1 ~ treatment_ad_exposures_week1 + as.factor(total_ad_exposures_week1))
        ## 
        ## Residuals:
        ##    Min     1Q Median     3Q    Max 
        ## -3.003 -2.102 -1.552  2.447  8.110 
        ## 
        ## Coefficients:
        ##                                      Estimate Std. Error t value Pr(>|t|)
        ## (Intercept)                          1.295932   0.011185  115.87   <2e-16
        ## treatment_ad_exposures_week1         0.056337   0.004609   12.22   <2e-16
        ## as.factor(total_ad_exposures_week1)1 0.256600   0.015295   16.78   <2e-16
        ## as.factor(total_ad_exposures_week1)2 0.487427   0.015402   31.65   <2e-16
        ## as.factor(total_ad_exposures_week1)3 0.693409   0.016194   42.82   <2e-16
        ## as.factor(total_ad_exposures_week1)4 0.919351   0.017469   52.63   <2e-16
        ## as.factor(total_ad_exposures_week1)5 1.136115   0.019287   58.91   <2e-16
        ## as.factor(total_ad_exposures_week1)6 1.368843   0.021886   62.55   <2e-16
        ##                                         
        ## (Intercept)                          ***
        ## treatment_ad_exposures_week1         ***
        ## as.factor(total_ad_exposures_week1)1 ***
        ## as.factor(total_ad_exposures_week1)2 ***
        ## as.factor(total_ad_exposures_week1)3 ***
        ## as.factor(total_ad_exposures_week1)4 ***
        ## as.factor(total_ad_exposures_week1)5 ***
        ## as.factor(total_ad_exposures_week1)6 ***
        ## ---
        ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
        ## 
        ## Residual standard error: 2.767 on 499992 degrees of freedom
        ## Multiple R-squared:  0.02783,    Adjusted R-squared:  0.02782 
        ## F-statistic:  2045 on 7 and 499992 DF,  p-value: < 2.2e-16

    **We can see that each treatment ad exposure is predicted to increase the purchase in week 1 by 0.0563 unit.**

6.  The colleague who proposed the specification in part (b) challenges your results -- they make the campaign look less successful. Write a paragraph that a layperson would understand about why your estimation strategy is superior and his/hers is biased. **When we see that our treatment is associated with pre-treatment purchase behaivior, we lose the ground to say that people in the treatment group's post-treatment behavior can be used to measure the effect of the ad. In our particular case, we see that people in the treatment group were more likely to purchase the treatment product before treatment was administered when analyzing the data naively. Therefore, even when we run a regression and see that people in the treatment group tended to buy more after receiving the treatment, we can't say that the additional purchase was caused by treatment since treatment group had a high tendency to purchase to begin with. Therefore, we need to control some variables in the analysis, so that treatment assignment isn't associated with pre-treatment purchase behavior anymore. Then, we can argue more logically that, any additional purchase in the treatment group after treatment administration was caused by the treatment.**

7.  Estimate the causal effect of each treatment ad exposure on purchases during and after the campaign, up until week 10 (so, total purchases during weeks 1 through 10).

    ``` r
    d[, week1_10 := rowSums(d[, 5:14])]

    mod_g <- d[, lm(week1_10 ~ treatment_ad_exposures_week1 +
                               as.factor(total_ad_exposures_week1))]
    summary(mod_g)
    ```

        ## 
        ## Call:
        ## lm(formula = week1_10 ~ treatment_ad_exposures_week1 + as.factor(total_ad_exposures_week1))
        ## 
        ## Residuals:
        ##     Min      1Q  Median      3Q     Max 
        ## -30.712  -7.363  -0.722   6.656  59.977 
        ## 
        ## Coefficients:
        ##                                      Estimate Std. Error t value Pr(>|t|)
        ## (Intercept)                          16.90169    0.04267 396.102   <2e-16
        ## treatment_ad_exposures_week1          0.01250    0.01758   0.711    0.477
        ## as.factor(total_ad_exposures_week1)1  2.59989    0.05835  44.555   <2e-16
        ## as.factor(total_ad_exposures_week1)2  4.84270    0.05876  82.418   <2e-16
        ## as.factor(total_ad_exposures_week1)3  7.03614    0.06178 113.891   <2e-16
        ## as.factor(total_ad_exposures_week1)4  9.08326    0.06664 136.297   <2e-16
        ## as.factor(total_ad_exposures_week1)5 11.19600    0.07358 152.157   <2e-16
        ## as.factor(total_ad_exposures_week1)6 13.73562    0.08350 164.508   <2e-16
        ##                                         
        ## (Intercept)                          ***
        ## treatment_ad_exposures_week1            
        ## as.factor(total_ad_exposures_week1)1 ***
        ## as.factor(total_ad_exposures_week1)2 ***
        ## as.factor(total_ad_exposures_week1)3 ***
        ## as.factor(total_ad_exposures_week1)4 ***
        ## as.factor(total_ad_exposures_week1)5 ***
        ## as.factor(total_ad_exposures_week1)6 ***
        ## ---
        ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
        ## 
        ## Residual standard error: 10.55 on 499992 degrees of freedom
        ## Multiple R-squared:  0.1322, Adjusted R-squared:  0.1322 
        ## F-statistic: 1.088e+04 on 7 and 499992 DF,  p-value: < 2.2e-16

    **We see that treatment doesn't have any significant effect on the cumulative purchase from week 1 to week 10.**

8.  Estimate the causal effect of each treatment ad exposure on purchases only after the campaign. That is, look at total purchases only during week 2 through week 10, inclusive.

    ``` r
    d[, week2_10 := rowSums(d[, 6:14])]

    mod_h <- d[, lm(week2_10 ~ treatment_ad_exposures_week1 + 
                               as.factor(total_ad_exposures_week1))]
    summary(mod_h)
    ```

        ## 
        ## Call:
        ## lm(formula = week2_10 ~ treatment_ad_exposures_week1 + as.factor(total_ad_exposures_week1))
        ## 
        ## Residuals:
        ##     Min      1Q  Median      3Q     Max 
        ## -27.973  -7.105  -0.705   6.394  54.266 
        ## 
        ## Coefficients:
        ##                                      Estimate Std. Error t value Pr(>|t|)
        ## (Intercept)                          15.60576    0.04087 381.800  < 2e-16
        ## treatment_ad_exposures_week1         -0.04384    0.01684  -2.603  0.00925
        ## as.factor(total_ad_exposures_week1)1  2.34329    0.05590  41.922  < 2e-16
        ## as.factor(total_ad_exposures_week1)2  4.35527    0.05628  77.379  < 2e-16
        ## as.factor(total_ad_exposures_week1)3  6.34273    0.05918 107.178  < 2e-16
        ## as.factor(total_ad_exposures_week1)4  8.16391    0.06384 127.884  < 2e-16
        ## as.factor(total_ad_exposures_week1)5 10.05988    0.07048 142.724  < 2e-16
        ## as.factor(total_ad_exposures_week1)6 12.36677    0.07998 154.621  < 2e-16
        ##                                         
        ## (Intercept)                          ***
        ## treatment_ad_exposures_week1         ** 
        ## as.factor(total_ad_exposures_week1)1 ***
        ## as.factor(total_ad_exposures_week1)2 ***
        ## as.factor(total_ad_exposures_week1)3 ***
        ## as.factor(total_ad_exposures_week1)4 ***
        ## as.factor(total_ad_exposures_week1)5 ***
        ## as.factor(total_ad_exposures_week1)6 ***
        ## ---
        ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
        ## 
        ## Residual standard error: 10.11 on 499992 degrees of freedom
        ## Multiple R-squared:  0.1156, Adjusted R-squared:  0.1156 
        ## F-statistic:  9333 on 7 and 499992 DF,  p-value: < 2.2e-16

    **We see that treatment has negative effect on the cumulative purchase from week 2 to week 10. Each unit of treatment is expected to decrease purchase unit by 0.044.**

9.  Tell a story that could plausibly explain the result from part (h). **In part (h), we see that being exposed to the treatment ads caused people to decrease their purchase of the treatment product during week 2-10. This can be explained by the fact that many of them made their purchse in the first week during the period of the campaign (since we see positive effect in part(e) )**

10. Test the hypothesis that the ads for product B are more effective, in terms of producing additional revenue in week 1 only, than are the ads for product A.

    ``` r
    mod_j <- d[, lm(week1 ~ treatment_ad_exposures_week1 + 
                            as.factor(total_ad_exposures_week1) +
                            product_b + 
                            product_b * treatment_ad_exposures_week1)]
    summary(mod_j)
    ```

        ## 
        ## Call:
        ## lm(formula = week1 ~ treatment_ad_exposures_week1 + as.factor(total_ad_exposures_week1) + 
        ##     product_b + product_b * treatment_ad_exposures_week1)
        ## 
        ## Residuals:
        ##    Min     1Q Median     3Q    Max 
        ## -3.027 -2.194 -1.506  2.434  8.079 
        ## 
        ## Coefficients:
        ##                                         Estimate Std. Error t value
        ## (Intercept)                             1.275600   0.011305  112.84
        ## treatment_ad_exposures_week1            0.059436   0.005727   10.38
        ## as.factor(total_ad_exposures_week1)1    0.230446   0.015530   14.84
        ## as.factor(total_ad_exposures_week1)2    0.447517   0.015905   28.14
        ## as.factor(total_ad_exposures_week1)3    0.647517   0.016816   38.51
        ## as.factor(total_ad_exposures_week1)4    0.866675   0.018144   47.77
        ## as.factor(total_ad_exposures_week1)5    1.069100   0.019945   53.60
        ## as.factor(total_ad_exposures_week1)6    1.269167   0.022573   56.23
        ## product_b                               0.164742   0.013513   12.19
        ## treatment_ad_exposures_week1:product_b -0.006460   0.006801   -0.95
        ##                                        Pr(>|t|)    
        ## (Intercept)                              <2e-16 ***
        ## treatment_ad_exposures_week1             <2e-16 ***
        ## as.factor(total_ad_exposures_week1)1     <2e-16 ***
        ## as.factor(total_ad_exposures_week1)2     <2e-16 ***
        ## as.factor(total_ad_exposures_week1)3     <2e-16 ***
        ## as.factor(total_ad_exposures_week1)4     <2e-16 ***
        ## as.factor(total_ad_exposures_week1)5     <2e-16 ***
        ## as.factor(total_ad_exposures_week1)6     <2e-16 ***
        ## product_b                                <2e-16 ***
        ## treatment_ad_exposures_week1:product_b    0.342    
        ## ---
        ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
        ## 
        ## Residual standard error: 2.766 on 499990 degrees of freedom
        ## Multiple R-squared:  0.02848,    Adjusted R-squared:  0.02847 
        ## F-statistic:  1629 on 9 and 499990 DF,  p-value: < 2.2e-16

(*Hint: The easiest way to do this is to throw all of the observations into one big regression and specify that regression in such a way that it tests this hypothesis.*)

(*Hint 2: There are a couple defensible ways to answer this question that lead to different answers. Don’t stress if you think you have an approach you can defend.*)

    __The result of the test didn't suggest the ads worked better with product b since in the regression, the coefficient for the interaction term between product b and treatment isn't significent.__

1.  You notice that the ads for product A included celebrity endorsements. How confident would you be in concluding that celebrity endorsements increase the effectiveness of advertising at stimulating immediate purchases? **Though the point estimate of the coefficient of the interaction term is negative, it is not significant. Even if it was significant and suggested that somehow the ad for product A was more effective in getting people to purchase, I would have little confidence in concluding it was the celebrity endorsement that made it more effective. There are simply too many unkown mechanisms or reasons that could have caused the difference in effectiveness.**

2. Vietnam Draft Lottery
========================

A [famous paper](http://sites.duke.edu/niou/files/2011/06/Angrist_lifetime-earningsmall.pdf) by Angrist exploits the randomized lottery for the Vietnam draft to estimate the effect of education on wages. (*Don’t worry about reading this article, it is just provided to satisfy your curiosity; you can answer the question below without referring to it. In fact, it may be easier for you not to, since he has some complications to deal with that the simple data we’re giving you do not.*)

Problem Setup
-------------

Angrist’s idea is this: During the Vietnam era, draft numbers were determined randomly by birth date -- the army would literally randomly draw birthdays out of a hat, and those whose birthdays came up sooner were higher up on the list to be drafted first. For example, all young American men born on May 2 of a given year might have draft number 1 and be the first to be called up for service, followed by November 13 who would get draft number 2 and be second, etc. The higher-ranked (closer to 1) your draft number, the likelier it was you would be drafted.

We have generated a fake version of this data for your use in this project. You can find real information (here)\[<https://www.sss.gov/About/History-And-Records/lotter1>\]. While we're defining having a high draft number as falling at 80, in reality in 1970 any number lower than 195 would have been a "high" draft number, in 1971 anything lower than 125 would have been "high".

High draft rank induced many Americans to go to college, because being a college student was an excuse to avoid the draft -- so those with higher-ranked draft numbers attempted to enroll in college for fear of being drafted, whereas those with lower-ranked draft numbers felt less pressure to enroll in college just to avoid the draft (some still attended college regardless, of course). Draft numbers therefore cause a natural experiment in education, as we now have two randomly assigned groups, with one group having higher mean levels of education, those with higher draft numbers, than another, those with lower draft numbers. (In the language of econometricians, we say the draft number is “an instrument for education,” or that draft number is an “instrumental variable.”)

Some simplifying assumptions:

-   Suppose that these data are a true random sample of IRS records and that these records measure every living American’s income without error.
-   Assume that the true effect of education on income is linear in the number of years of education obtained.
-   Assume all the data points are from Americans born in a single year and we do not need to worry about cohort effects of any kind.

<!-- -->

    ##    draft_number years_education    income
    ## 1:          267              16  44573.90
    ## 2:          357              13  10611.75
    ## 3:          351              19 165467.80
    ## 4:          205              16  71278.40
    ## 5:           42              19  54445.09
    ## 6:          240              11  32059.12

Questions to Answer
-------------------

1.  Suppose that you had not run an experiment. Estimate the "effect" of each year of education on income as an observational researcher might, by just running a regression of years of education on income (in R-ish, `income ~ years_education`). What does this naive regression suggest?

    ``` r
    mod_a <- d[, lm(income ~ years_education)]
    summary(mod_a)
    ```

        ## 
        ## Call:
        ## lm(formula = income ~ years_education)
        ## 
        ## Residuals:
        ##    Min     1Q Median     3Q    Max 
        ## -91655 -17459   -837  16346 141587 
        ## 
        ## Coefficients:
        ##                  Estimate Std. Error t value Pr(>|t|)    
        ## (Intercept)     -23354.64    1252.74  -18.64   <2e-16 ***
        ## years_education   5750.48      83.34   69.00   <2e-16 ***
        ## ---
        ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
        ## 
        ## Residual standard error: 26590 on 19565 degrees of freedom
        ## Multiple R-squared:  0.1957, Adjusted R-squared:  0.1957 
        ## F-statistic:  4761 on 1 and 19565 DF,  p-value: < 2.2e-16

    **We can see that the naive regression suggests each additional year of education generates around 5750 dollars of additional income.**

2.  Continue to suppose that we did not run the experiment, but that we saw the result that you noted in part (a). Tell a concrete story about why you don't believe that observational result tells you anything causal. **In this case, I would argue that there were omitted variables such as intellectual ability that was positively correlated to both education and income. Therefore, with this variable missing from the regression, we could overestimate the effect of education and we couldn't be sure whether education really had an effect on income.**

3.  Now, let’s get to using the natural experiment. We will define “having a high-ranked draft number” as having a draft number of 80 or below (1-80; numbers 81-365, for the remaining 285 days of the year, can be considered “low-ranked”). Create a variable in your dataset indicating whether each person has a high-ranked draft number or not. Using regression, estimate the effect of having a high-ranked draft number, the dummy variable you’ve just created, on years of education obtained. Report the estimate and a correctly computed standard error. (\*Hint: Pay special attention to calculating the correct standard errors here. They should match how the draft is conducted.)

    ``` r
    d[, high_ranked := ifelse(draft_number <= 80, 1, 0)]
    mod_c <- d[, lm(years_education ~ high_ranked)]
    # Calculate clustered se
    coeftest(mod_c, vcovCL(mod_c, cluster = d[ , draft_number]))
    ```

        ## 
        ## t test of coefficients:
        ## 
        ##              Estimate Std. Error t value  Pr(>|t|)    
        ## (Intercept) 14.434305   0.017703 815.345 < 2.2e-16 ***
        ## high_ranked  2.125756   0.038188  55.666 < 2.2e-16 ***
        ## ---
        ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    **Since the randomization is clustered based on people having the same birthday, we will calculate the clustered standard error for the coefficient of variable `high_ranked`. The regression estimated being in the high-ranked group made people have on average 2.12 years of additional education with a standard error of 0.038 years. It's highly significant.**

4.  Using linear regression, estimate the effect of having a high-ranked draft number on income. Report the estimate and the correct standard error.

    ``` r
    mod_d <- d[, lm(income ~ high_ranked)]
    # Calculate clustered se
    coeftest(mod_d, vcovCL(mod_d, cluster = d[ , draft_number]))
    ```

        ## 
        ## t test of coefficients:
        ## 
        ##             Estimate Std. Error t value  Pr(>|t|)    
        ## (Intercept) 60761.89     244.36 248.656 < 2.2e-16 ***
        ## high_ranked  6637.55     511.90  12.966 < 2.2e-16 ***
        ## ---
        ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    **The result of the regression suggests being in the high-ranked group, people earned 6637 dollars more with a standard error of 512 dollars.**

5.  Divide the estimate from part (d) by the estimate in part (c) to estimate the effect of education on income. This is an instrumental-variables estimate, in which we are looking at the “clean” variation in both education and income that is due to the draft status, and computing the slope of the income-education line as “clean change in Y” divided by “clean change in X”. What do the results suggest?

    ``` r
    mod_d$coefficients[2]/mod_c$coefficients[2]
    ```

        ## high_ranked 
        ##    3122.444

    **The results suggests each additional year of aducation produces about 3122 dollars of additional income.**

6.  Natural experiments rely crucially on the “exclusion restriction” assumption that the instrument (here, having a high draft rank) cannot affect the outcome (here, income) in any other way except through its effect on the “endogenous variable” (here, education). Give one reason this assumption may be violated -- that is, why having a high draft rank could affect individuals’ income other than because it nudges them to attend school for longer. \_\_\_\_

7.  Conduct a test for the presence of differential attrition by treatment condition. That is, conduct a formal test of the hypothesis that the “high-ranked draft number” treatment has no effect on whether we observe a person’s income. **(Note, that an earning of $0 *actually* means they didn't earn any money.)**

    **Since the birthdays are randomly selected to be in the treatment and control groups, we could say any birthday are equally likely to be in high-ranked or low-ranked groups. This means we expect the mean numbers of people born a day in the two group to be the same. We can use this to test whether one group has higher attrition than the other. The reason being that the two groups are expected to have the same number of people born on a given day, however, if one group has that number lower than the other with statistical significance, we know that there is differential attrition since one group have more people missing whose data should have been available to us.**

    ``` r
    # get number of birth for a day
    count_by_draft_number <- d[, .(n = .N), keyby= .(draft_number, high_ranked)]
    # t-test the averag number of birth in high-ranked vs low-ranked
    count_by_draft_number[, t.test(n ~ high_ranked)]
    ```

        ## 
        ##  Welch Two Sample t-test
        ## 
        ## data:  n by high_ranked
        ## t = 6.6358, df = 123.31, p-value = 9.121e-10
        ## alternative hypothesis: true difference in means is not equal to 0
        ## 95 percent confidence interval:
        ##  4.410934 8.160996
        ## sample estimates:
        ## mean in group 0 mean in group 1 
        ##        54.98596        48.70000

    **The t-test suggests that the high-ranked groups has fewer people born on a day with a p-value of 9.121e-10. Therefore, we know that the high-ranked group has more attrition than the low-ranked group.**
8.  Tell a concrete story about what could be leading to the result in part (g). **It could be that the high-ranked group had more people joining the war. Due to casualty and PTSD, more of them may not have the opportunity to work and have their income recorded by the IRS.**

3. Optional: Think about Treatment Effects
==========================================

Throughout this course we have focused on the average treatment effect. Think back to *why* we are concerned about the average treatment effect. What is the relationship between an ATE, and some individuals' potential outcomes? Make the strongest case you can for why this is a *good* measure.
