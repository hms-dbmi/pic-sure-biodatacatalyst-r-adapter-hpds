pic-sure-biodatacatalyst-r-adapter-hpds

# Adapter Development
Generally, `devtools::check()` can be used in the R shell to check that documentation is up to date, unit tests are passing with a clean environment, and other possible problems.

## Local Mac Installation
Install libgit2 locally, so devtools can be installed.

```
brew install R libgit2
R
Sys.setenv(TAR = "/usr/bin/tar")
options(unzip = "internal")
install.packages("devtools")
install.packages("urltools")
devtools::install_github("hms-dbmi/pic-sure-r-client", force=T)
devtools::install_github("hms-dbmi/pic-sure-r-adapter-hpds", force=T)
devtools::install("<full path to your local repo copy>/pic-sure-biodatacatalyst-r-adapter-hpds", force=T)
```

## Running unit tests
Start `R` in this directory and run `devtools::test()` to execute all the unit tests available. For more information on unit testing in R, see [R-Pkg's testing chapter](https://r-pkgs.org/tests.html) and [R-Lib's testthat package](https://testthat.r-lib.org/index.html).

## Run pseudo tests (manual integration)
Start `R` on the command line and install the latest copy of the library. Then source the pseudo tests, `source("pseudo-tests/quick-check.R")` and run them with a pic-sure url and your auth token, `runtest(url, token)`.

## Documentation
Documentation is generated from roxygen comments. To update, start `R` in this directory and run `devtools::document()` to regenerate the manual pages.
