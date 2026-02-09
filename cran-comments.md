## Test environments
* local R installation, R 4.5.2
* windows, macOS, ubuntu on Github Actions as set with usethis::use_github_action_check_standard() - passing.
* devtools::check_win_devel()

## R CMD check results

0 errors | 0 warnings | 0 note

## Comments

* drops dependency also on `wikidataR`, now archived on CRAN, after dropping dependency on `WikidataQueryServiceR` (also archived) in version 0.6.0, released just recently.
