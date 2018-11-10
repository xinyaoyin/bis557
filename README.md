[![Build Status](https://travis-ci.com/xinyaoyin/bis557.svg?branch=master)](https://travis-ci.com/xinyaoyin/bis557)

BIS557
===

This is a repository for storing all code, documentation, and digital 
artifacts for BIS557.

Homework 1 
So far the only thing we've done is create and document a function that
calls `lm`. You can use it like this:

```{R}
library(bis557)
fit <- linear_model(Sepal.Length ~., iris)
summary(fit)
```

Homework 2
We've created the ridge_reg function. You can use it like this:
```{R}
library(bis557)
fit <- ridge_reg(Sepal.Length ~ .,1.2121, iris)
summary(fit)
```
We've also created a vignette, called homework-2, that looks at effect on the out-of-sample mean square error as lambda varies. 


Homework 3 
Finished homework 3, constructed Epanechnikov kernel function and tested the KKT conditions for glmnet.