
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CPRDTools

<!-- badges: start -->
<!-- badges: end -->

This wrapper set of functions hopes to aid the loading, handling,
extraction and all round wrangling of Clinical Practice Research
Datalink (CPRD) electronic health records data and its associated linked
and any peripheral data. This is achieved through the automated loading
of CPRD GOLD datasets into SQLite database, specifically the Additional,
Clinical, Consultation, Immunisation, Patient, Practice, Referral,
Staff, Test and Therapy datasets. The package relies on the consistent
naming of CPRD datasets and the singular location of the raw data files.

Extracting data from the database can be achieved with a user defined
data query, either using a simplified pre-defined structure or through
the use of a SQL defined code. The SQL code provides the most
customisable, flexible and comprehensive data extraction method but does
require knowledge of SQL code, a great introduction can be found at
<https://www.sqlitetutorial.net/sqlite-select/>.

You can install the development version of CPRDTools from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("JamesCFSchmidt/CPRDTools")
```
