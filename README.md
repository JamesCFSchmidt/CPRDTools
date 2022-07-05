
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CPRDTools

<!-- badges: start -->
<!-- badges: end -->

This wrapper set of functions hopes to aid the loading, handling,
extraction and all round wrangling of Clinical Practice Research
Datalink (CPRD) electronic health records data and its associated linked
and peripheral data. A database provides an efficient, secure and
updatable location, allowing the original CPRD GOLD data to remain
untouched while the database contains the unzipped and appended data.
The extraction of data from a database via a data query allows for the
joining, filtering and sub-setting of the required data in a single
step. The functions within CPRDTools fall within three working areas (1)
Loading, (2) Maintenance and other tasks and (3) Extraction.

1)  Loading: Loading is achieved through the automated loading of CPRD
    GOLD datasets into a SQLite database, specifically the Additional,
    Clinical, Consultation, Immunisation, Patient, Practice, Referral,
    Staff, Test and Therapy datasets. The package relies on the
    consistent naming of CPRD datasets and the singular location of the
    raw data files. Additional and peripheral data can be loaded into
    the dataset.

2)  Maintenance and other tasks: Provided for in CPRDTools are function
    to help maintain and improve the database, from query speed
    improvement to date conversions.

3)  Extraction: Extracting data from the database can be achieved with a
    user defined data query, either using a simplified pre-defined
    structure or through the use of a SQL defined code. The SQL code
    provides the most customisable, flexible and comprehensive data
    extraction method but does require knowledge of SQL code, a great
    introduction can be found at
    <https://www.sqlitetutorial.net/sqlite-select/>.

You can install the development version of CPRDTools from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("JamesCFSchmidt/CPRDTools")
```
