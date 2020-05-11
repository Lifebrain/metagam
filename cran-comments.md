## Comments

This is a resubmission containing:

* A small internal fix for compatibility with the upcoming dplyr v1.0.0.
* Removal of 'tibble' from Imports, as suggested by a current NOTE on CRAN.
* Removal of unit tests failing in MKL because of issues related to random number generation, as suggested by a current MKL on CRAN.

## Test environments
* local OS X install, R 4.0
* ubuntu 16.04 (on travis-ci), R 4.0

## R CMD check results

0 ERRORs | 0 WARNINGs | 0 NOTEs
