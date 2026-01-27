# Set language to be used by all functions

Defaults to current package name (`tidywikidatar`) and version.

## Usage

``` r
tw_set_user_agent(user_agent)

tw_get_user_agent(user_agent)
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
#> Error in tw_get_user_agent(): argument "user_agent" is missing, with no default
default_user_agent
#> Error: object 'default_user_agent' not found
# Custom user agent
tw_set_user_agent(user_agent = "custom_project_name/email")
new_user_agent <- tw_get_user_agent()
#> Error in tw_get_user_agent(): argument "user_agent" is missing, with no default
new_user_agent
#> Error: object 'new_user_agent' not found
# Restore
tw_set_user_agent(user_agent = default_user_agent)
#> Error: object 'default_user_agent' not found
tw_get_user_agent()
#> Error in tw_get_user_agent(): argument "user_agent" is missing, with no default
```
