Purpose
-------

This R package allows to conduct statistical inference (hypotheses
testing and construction of confidence intervals) regarding the unknown
performances of (machine-learned) prediction models. The main goal is
(approximately) control the type 1 error rate of the employed
statistical tests to obtain a reliable performance assessment.

Several simulation studies have been conducted with the current version
of the package, compare
<a href="https://maxwestphal.github.io/SEPM.PUB/" class="uri">https://maxwestphal.github.io/SEPM.PUB/</a>.
However, this package is still under development.

Getting Started
---------------

The package may be installed by using the R command

``` r
devtools::install_github("https://github.com/maxwestphal/SEPM")
```

Additional information
----------------------

The package is still under development, several aspects will be improved
/ added in the future. The main evaluation chain is already supposed to
work properly for most situations.

SEPM provides little completely new functionality. One exeption is the
simultaneous inference regarding co-primary endpoints sensitivity and
specificity for multiple classifiers. However, as the package is
tailored explcitly for the planning and analysis of model evaluation
studies, the code efficiency should be reduced compared to using base R
in conjunction with other packages for this purpose.
