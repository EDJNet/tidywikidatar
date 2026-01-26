# Checks if cache folder exists, if not returns an informative message

Checks if cache folder exists, if not returns an informative message

## Usage

``` r
tw_check_cache_folder()
```

## Value

If the cache folder exists, returns `TRUE`. Otherwise throws an error.

## Examples

``` r
# If cache folder does not exist, it throws an error
tryCatch(tw_check_cache_folder(),
  error = function(e) {
    return(e)
  }
)
#> <error/rlang_error>
#> Error in `tw_check_cache_folder()`:
#> ! Cache folder does not exist.
#> ℹ Set it with `tw_set_cache_folder()` and create it with
#>   `tw_create_cache_folder()`.
#> ---
#> Backtrace:
#>      ▆
#>   1. └─pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)
#>   2.   └─pkgdown::build_site(...)
#>   3.     └─pkgdown:::build_site_local(...)
#>   4.       └─pkgdown::build_reference(...)
#>   5.         ├─pkgdown:::unwrap_purrr_error(...)
#>   6.         │ └─base::withCallingHandlers(...)
#>   7.         └─purrr::map(...)
#>   8.           └─purrr:::map_("list", .x, .f, ..., .progress = .progress)
#>   9.             ├─purrr:::with_indexed_errors(...)
#>  10.             │ └─base::withCallingHandlers(...)
#>  11.             ├─purrr:::call_with_cleanup(...)
#>  12.             └─pkgdown (local) .f(.x[[i]], ...)
#>  13.               ├─base::withCallingHandlers(...)
#>  14.               └─pkgdown:::data_reference_topic(...)
#>  15.                 └─pkgdown:::run_examples(...)
#>  16.                   └─pkgdown:::highlight_examples(code, topic, env = env)
#>  17.                     └─downlit::evaluate_and_highlight(...)
#>  18.                       └─evaluate::evaluate(code, child_env(env), new_device = TRUE, output_handler = output_handler)
#>  19.                         ├─base::withRestarts(...)
#>  20.                         │ └─base (local) withRestartList(expr, restarts)
#>  21.                         │   ├─base (local) withOneRestart(withRestartList(expr, restarts[-nr]), restarts[[nr]])
#>  22.                         │   │ └─base (local) doWithOneRestart(return(expr), restart)
#>  23.                         │   └─base (local) withRestartList(expr, restarts[-nr])
#>  24.                         │     └─base (local) withOneRestart(expr, restarts[[1L]])
#>  25.                         │       └─base (local) doWithOneRestart(return(expr), restart)
#>  26.                         ├─evaluate:::with_handlers(...)
#>  27.                         │ ├─base::eval(call)
#>  28.                         │ │ └─base::eval(call)
#>  29.                         │ └─base::withCallingHandlers(...)
#>  30.                         ├─base::withVisible(eval(expr, envir))
#>  31.                         └─base::eval(expr, envir)
#>  32.                           └─base::eval(expr, envir)
#>  33.                             ├─base::tryCatch(...)
#>  34.                             │ └─base (local) tryCatchList(expr, classes, parentenv, handlers)
#>  35.                             │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
#>  36.                             │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
#>  37.                             └─tidywikidatar::tw_check_cache_folder()

# Create cache folder
tw_set_cache_folder(path = fs::path(
  tempdir(),
  "tw_cache_folder"
))
tw_create_cache_folder(ask = FALSE)

tw_check_cache_folder()
#> [1] TRUE
```
