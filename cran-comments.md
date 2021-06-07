## Test environments
* local R installation, R 4.0.4
* win-builder (devel)
* windows, macOS, ubuntu 20.04 on Github Actions as set with usethis::use_github_action_check_standard() - passing.
* rhub::check_for_cran()
* devtools::check_win_devel()

## R CMD check results

0 errors | 0 warnings | 0 note

* This is a new release.

## Additional details
* informative message when server unavailable, as requested
* removed tests with inconsistent results
