

==> devtools::check(document = FALSE)

Setting env vars --------------------------------------------------------------
CFLAGS  : -Wall -pedantic
CXXFLAGS: -Wall -pedantic
Building SubgrPlots -----------------------------------------------------------
'/usr/lib/R/bin/R' --no-site-file --no-environ --no-save --no-restore --quiet  \
  CMD build '/home/nico/Rpackages/SubgrPlots' --no-resave-data --no-manual 

* checking for file ‘/home/nico/Rpackages/SubgrPlots/DESCRIPTION’ ... OK
* preparing ‘SubgrPlots’:
* checking DESCRIPTION meta-information ... OK
* checking for LF line-endings in source and make files and shell scripts
* checking for empty or unneeded directories
* looking to see if a ‘data/datalist’ file should be added
* building ‘SubgrPlots_0.1.0.tar.gz’

Setting env vars --------------------------------------------------------------
_R_CHECK_CRAN_INCOMING_ : FALSE
_R_CHECK_FORCE_SUGGESTS_: FALSE
Checking SubgrPlots -----------------------------------------------------------
'/usr/lib/R/bin/R' --no-site-file --no-environ --no-save --no-restore --quiet  \
  CMD check '/tmp/RtmpvxfPqj/SubgrPlots_0.1.0.tar.gz' --as-cran --timings  \
  --no-manual 

* using log directory ‘/home/nico/Rpackages/SubgrPlots.Rcheck’
* using R version 3.4.2 (2017-09-28)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --as-cran’
* checking for file ‘SubgrPlots/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘SubgrPlots’ version ‘0.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... NOTE
Package suggested but not available for checking: ‘rgeos’
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘SubgrPlots’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd line widths ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking examples ... OK
* DONE
Status: 1 NOTE

See
  ‘/home/nico/Rpackages/SubgrPlots.Rcheck/00check.log’
for details.


checking package dependencies ... NOTE
Package suggested but not available for checking: ‘rgeos’
R CMD check results
0 errors | 0 warnings | 1 note 

R CMD check succeeded
