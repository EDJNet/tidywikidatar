## Test environments
* local R installation, R 4.5.3
* windows, macOS, ubuntu on Github Actions as set with usethis::use_github_action_check_standard() - passing.
* devtools::check_win_devel()

## R CMD check results

0 errors | 0 warnings | 0 note

## Comments

* reduces calls to API in examples due to more restrictive Wikidata API policies
