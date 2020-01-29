==> devtools::check(document = FALSE)

── Building ────────────────────────────── SubgrPlots ──
Setting env vars:
● CFLAGS    : -Wall -pedantic -fdiagnostics-color=always
● CXXFLAGS  : -Wall -pedantic -fdiagnostics-color=always
● CXX11FLAGS: -Wall -pedantic -fdiagnostics-color=always
────────────────────────────────────────────────────────
✔  checking for file ‘/Users/nicolasballarini/SubgrPlots/DESCRIPTION’ (364ms)
─  preparing ‘SubgrPlots’: (3.1s)
✔  checking DESCRIPTION meta-information ...
─  installing the package to build vignettes (636ms)
✔  creating vignettes (2m 14.5s)
─  checking for LF line-endings in source and make files and shell scripts (1s)
─  checking for empty or unneeded directories
─  looking to see if a ‘data/datalist’ file should be added
─  building ‘SubgrPlots_0.1.3.tar.gz’
   
── Checking ────────────────────────────── SubgrPlots ──
Setting env vars:
● _R_CHECK_CRAN_INCOMING_REMOTE_: FALSE
● _R_CHECK_CRAN_INCOMING_       : FALSE
● _R_CHECK_FORCE_SUGGESTS_      : FALSE
── R CMD check ─────────────────────────────────────────────────────────────────
* using log directory ‘/Users/nicolasballarini/SubgrPlots.Rcheck’
* using R version 3.5.3 (2019-03-11)
* using platform: x86_64-apple-darwin15.6.0 (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --as-cran’
* checking for file ‘SubgrPlots/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘SubgrPlots’ version ‘0.1.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking serialization versions ... OK
* checking whether package ‘SubgrPlots’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
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
* checking sizes of PDF files under ‘inst/doc’ ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
** found \donttest examples: check also with --run-donttest
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking re-building of vignette outputs ... OK
* DONE

Status: OK

── R CMD check results ─────────────────────────────────── SubgrPlots 0.1.3 ────
Duration: 4m 37.3s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded
