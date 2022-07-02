---
title: "Simultaneous euqtions model"
output: html_document
---

```{r setup, include=FALSE}
library(devtools)
library(usethis)
# install_git("https://github.com/ccolonescu/PoEdata")
library(PoEdata)
library(broom)
library(knitr)
library(systemfit)

data("fultonfish", package="PoEdata")
```


```{r fishQ.ols, echo=TRUE}
fishQ.ols <- lm(lquan~mon+tue+wed+thu+stormy, data=fultonfish)
kable(tidy(fishQ.ols), digits=4,
      caption="Reduced 'Q' equation for the fultonfish example")
```


```{r fishP.ols, echo=TRUE}
fishP.ols <- lm(lprice~mon+tue+wed+thu+stormy, data=fultonfish)
kable(tidy(fishP.ols), digits=4,
      caption="Reduced 'P' equation for the fultonfish example")
```


***2SLS***
```{r fish.sys, echo=TRUE}
fish.D <- lquan~lprice+mon+tue+wed+thu
fish.S <- lquan~lprice+stormy
fish.eqs <- list(fish.D, fish.S)
fish.ivs <- ~mon+tue+wed+thu+stormy
fish.sys <- systemfit(fish.eqs, method="2SLS", 
                      inst=fish.ivs, data=fultonfish)
summary(fish.sys)

```

