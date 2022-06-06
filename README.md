pic-sure-biodatacatalyst-r-adapter-hpds

# Local Mac Installation
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