# Set language to be used by all functions

Defaults to current package name (`tidywikidatar`) and version.

## Usage

``` r
tw_set_user_agent(user_agent = NULL)

tw_get_user_agent(user_agent = NULL)
```

## Arguments

- user_agent:

  Defaults to `NULL`. If not given, implicitly defaults to current
  package name (`tidywikidatar`) and version.

## Value

The user agent set for the session, implicitly.

## Examples

``` r
# Default user agent
default_user_agent <- tw_get_user_agent()
default_user_agent
#> [1] "tidywikidatar/0.6"
# Custom user agent
tw_set_user_agent(user_agent = "custom_project_name/email")
new_user_agent <- tw_get_user_agent()
new_user_agent
#> [1] "custom_project_name/email"
# Restore
tw_set_user_agent(user_agent = default_user_agent)
tw_get_user_agent()
```
