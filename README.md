
<!-- README.md is generated from README.Rmd. Please edit that file -->

# parsenet.pg

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of parsenet.pg is to …

## Installation

You can install the development version from GitHub with:

``` r
install.packages("remotes")
remotes::install_github("chenh2017/parsenet.pg")
```

## Usage

### Build the database

``` r
library(parsenet.pg)
# required steps: df_edges and dict
data2db(data, "df_edges", db)
data2db(data, "dict", db)
# optional steps: some other data
data2db(data, "name of the data", db, title = "the title of the table", note = "description of the data")
```

### Set environment variables

.Renviron is a user-controllable file that can be used to create
environment variables. This is especially useful to avoid including
credentials like API keys inside R scripts.

#### Open .Renviron

``` r
# opens .Renviron
usethis::edit_r_environ()
```

#### Set environment variable

.Renviron is written in a key-value format, so environment variables are
created in the format:

    DB_HOST='host'
    DB_PORT=port
    DB_USER='username'
    DB_PSWD='password'

### Run app

This is a basic example which shows you how to run the `parsenet.pg`
app. Remember you need to get access to the data and save it to your
local computer. In order to guarantee some dependencies are loaded, you
must use `library(parsenet.pg)` beforehand, instead of directly running
`parsenet.pg::run_app()`.

``` r
library(parsenet.pg)
run_app(db = "name of the database")
```

See the [getting started
guide](https://chenh2017.github.io/parsenet.pg/articles/main.html) to
learn how to use parsenet.pg.
