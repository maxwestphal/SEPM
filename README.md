---
title: "SEPM: Statistical Evaluation of Prediction Models (R package)"
author: "Max Westphal, Institute for Statistics, University of Bremen, Germany (mwestphal@uni-bremen.de)"
date: "June 7, 2019"
output: html_document
---

## Purpose

This R package allows to conduct statistical inference (hypotheses testing and 
construction of confidence intervals) regarding the unknown performances of (machine-learned)
prediction models. The main goal is (approximately) control the type 1 error rate of the employed
statistical tests to obtain a reliable performance assessment.

Several simulation studies have been conducted with the current version of the package, compare https://maxwestphal.github.io/SEPM.PUB/. 
However, this package is still under development.

## Getting Started


## Additional information

The package is still under development, several aspects will be improved / added in the future.
The main evaluation chain is already supposed to work properly for most situations.

SEPM provides little completely new functionality. One exeption is the simultaneous inference regarding co-primary endpoints sensitivity and specificity for multiple classifiers. However, as
the package is tailored explcitly for the analysis of model evaluation studies. In this regard the expected amount of needed code should be reduced compared to using base R and/or other packages.







