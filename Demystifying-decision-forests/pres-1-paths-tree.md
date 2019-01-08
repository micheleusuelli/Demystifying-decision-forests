
Wordcount-based clustering
========================================================
author: Michele Usuelli
date: 2016-01-29
width: 1680
height: 1050
css: css/custom.css















Load the data
====

The data set is "mortDefaultSmall" with simulated data on mortgage defaults. Each record corresponds to a customer of a bank and each column could be a predictor for a default.


```
  creditScore houseAge yearsEmploy ccDebt year default    id
1         721       19           4    958 2003       0 37089
2         725       23           3   3603 2005       0 57076
3         730       11           6   5570 2009       0 90649
4         649       14           6   5243 2002       0 20110
5         761       17           6   5189 2008       0 89640
6         685       29           9   5315 2009       0 94359
```




Build a decision tree
====

Using the function rxDTree, we build a decision tree predicting the mortgages default on the base of the chosen predictors.


```r
model_tree <- rxDTree(
  formula = formula_predict, 
  data = df_train
)
```

![plot of chunk plot-tree](pres-1-paths-tree-figure/plot-tree-1.png) 




Extract the frame of the tree
====


```
     predictor  value       score
1       ccDebt 6810.0 0.487383798
2         year 2007.5 0.065868263
3  yearsEmploy    1.5 0.003891051
4       <leaf>     NA 0.000000000
5       <leaf>     NA 0.100000000
6       ccDebt 5510.0 0.272727273
7  yearsEmploy    3.5 0.021739130
8       <leaf>     NA 0.000000000
9       <leaf>     NA 0.083333333
10 yearsEmploy    4.5 0.645161290
```




Define the paths for the test set
====


```
      id   predictor  value       score go_left
1: 90649      ccDebt 6810.0 0.487383798    TRUE
2: 90649        year 2007.5 0.065868263   FALSE
3: 90649      ccDebt 5510.0 0.272727273   FALSE
4: 90649 yearsEmploy    4.5 0.645161290   FALSE
5: 37870      ccDebt 6810.0 0.487383798    TRUE
6: 37870        year 2007.5 0.065868263    TRUE
7: 37870 yearsEmploy    1.5 0.003891051   FALSE
```






Measure the impact of the features on each record of the test set
====


![plot of chunk measure-relevance](pres-1-paths-tree-figure/measure-relevance-1.png) 




Measure the impact of each split across all the test set
====



```r
df_thresholds <- df_splits %>% 
  group_by(var, index) %>% 
  summarise(ydiff = mean(ydiff), 
            n_splits = ydiff %>% unique %>% length) %>% 
  arrange(-abs(ydiff))
df_thresholds
```

```
Source: local data table [12 x 4]
Groups: var

           var   index       ydiff n_splits
         (chr)   (dbl)       (dbl)    (int)
1       ccDebt  6810.0 -0.30788742        2
2       ccDebt  8910.0 -0.13462785        2
3       ccDebt  5510.0 -0.04318076        2
4       ccDebt 10060.0 -0.03059581        1
5       ccDebt  7830.0  0.02758563        2
6  creditScore   663.0  0.00000000        1
7     houseAge    22.5  0.05847953        1
8         year  2005.5  0.08695652        1
9         year  2007.5 -0.01599266        3
10 yearsEmploy     1.5  0.09610895        1
11 yearsEmploy     3.5  0.06159420        1
12 yearsEmploy     4.5 -0.03618693        2
```
