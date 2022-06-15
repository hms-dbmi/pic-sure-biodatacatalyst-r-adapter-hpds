pic-sure-biodatacatalyst-r-adapter-hpds

# Adapter Development

## Local Mac Installation
Install libgit2 locally, so devtools can be installed.

```
brew install R libgit2
R
install.packages("devtools")
install.packages("urltools")
Sys.setenv(TAR = "/usr/bin/tar")
options(unzip = "internal")
devtools::install_github("hms-dbmi/pic-sure-r-client", force=T)
devtools::install_github("hms-dbmi/pic-sure-r-adapter-hpds", force=T)
devtools::install("<full path to your local repo copy>/pic-sure-biodatacatalyst-r-adapter-hpds", force=T)
```

## Running unit tests
Start `R` on the command line in this repo and run `devtools::test()` to run all the unit tests available. For more information on unt testing in R, see [R-Pkg's testing chapter](https://r-pkgs.org/tests.html) and [R-Lib's testthat package](https://testthat.r-lib.org/index.html).

## Documentation
Documentation is generated from roxygen comments. To update, start `R` in this repo and run `devtools::document()` to regenerate the manual pages.