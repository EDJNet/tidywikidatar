
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidywikidatar

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of `tidywikidatar` is to facilitate interaction with Wikidata:

  - all responses are transformed into data frames
  - by default, queries and responses are cached locally in a sqlite
    database

## Installation

You can install `tidywikidatar` from
[Github](https://github.com/giocomai/tidywikidatar) with:

``` r
# install.packages("remotes")
remotes::install_github("giocomai/tidywikidatar")
```

## Before you start

This package assumes some familiarity with basic Wikidata concepts. For
reference, see [the introduction on the official
website](https://www.wikidata.org/wiki/Wikidata:Introduction).

At the most basic, you should know that every item in Wikidata has an id
(it always starts with a Q, something like `Q123456`). Each item is
described by properties (they always start with a P something like
`P1234`).

So for example, if I am interested in the anthropologist Margert Mead, I
will search her name on Wikidata and discover that she is
[`Q180099`](https://www.wikidata.org/wiki/Q180099). She is described by
many properties. For example, she is “an instance of”
([P31](https://www.wikidata.org/wiki/Property:P31))
“[Q5](https://www.wikidata.org/wiki/Q180099)”, which means “human”.
Her “sex or gender” ([P21](https://www.wikidata.org/wiki/Property:P21))
is “[Q180099](https://www.wikidata.org/wiki/Q6581072)”, which means,
female. By “occupation”
([P106](https://www.wikidata.org/wiki/Property:P106)), she was
“[Q36180](https://www.wikidata.org/wiki/Q36180)”,
“[Q4773904](https://www.wikidata.org/wiki/Q4773904)”, and
“[Q674426](https://www.wikidata.org/wiki/Q674426)”, which means, a
writer, an anthropologist, and a curator. And so forth.

As you’ll see, many queries return just another wikidata id, and if you
want to know what that means, you’ll need to ask for what that id stands
for.

## How to use

By default, `tidywikidatar` caches locally responses (both searches and
details about specific items) in a sqlite database to reduce load on
Wikidata’s servers. These sqlite databases are by default stored in the
current working directory under a `tw_data` folder. I usually store them
in a folder where they can be retrieved easily even when working on
different projects, but this is obviously a matter of personal taste.
You can set the cache folder to be used throughout a session with
`tw_set_cache_folder()`.

``` r
library("tidywikidatar")
tw_set_cache_folder(path = "~/R/tw_data/")
#> [1] "~/R/tw_data/"
```

## Finding details about something

Most `tidywikidatar` functions are built around the idea that you know
what you are looking for, and just want to get what Wikidata knows about
it, assuming the preferred choice would be among the top results.

Let’s say I am interested in Margaret Mead, the famous pioneer
anthropologist author of “Coming of Age in Samoa”.

``` r
tw_search(search = "Margaret Mead")
#>           id                                                      label
#> 1    Q180099                                              Margaret Mead
#> 2  Q81015029                                              Margaret mead
#> 3  Q66701460                                              Margaret Mead
#> 4  Q85724626                                             Mead & Bateson
#> 5  Q96077616                                           Margaret Meadows
#> 6  Q76238541                                           Margaret Meadowe
#> 7  Q75506638                                           Margaret Meadows
#> 8  Q75812372                                       Margaret Meade-Waldo
#> 9   Q6759717                                Margaret Mead Film Festival
#> 10 Q55897055 Margaret Mead and Samoa: Coming of Age in Fact and Fiction
#>                                         description
#> 1                           American anthropologist
#> 2  scientific article published on 01 December 1978
#> 3  scientific article published on 01 November 1978
#> 4                             business organisation
#> 5                                              <NA>
#> 6                          Peerage person ID=628312
#> 7                          Peerage person ID=183057
#> 8                                         died 1954
#> 9   annual film festival held in New York City, USA
#> 10                                             <NA>
```

This seems quite straightforward but there are actually a number of
things that are returned by searching for “Margaret Mead”.

If I am running through a list of strings, and, for example, I am
actually interested in the most famous person by that name, I can filter
result by property, using the standard form. If, for example, I want
only the first result that is associated with “an instance of” (P31) -
“human” (Q5), I can run:

``` r
tw_search(search = "Margaret Mead") %>% 
  tw_filter_first(p = "P31", q = "Q5")
#>        id         label             description
#> 1 Q180099 Margaret Mead American anthropologist
```

Where was she born? I can ask directly for P19, place of birth:

``` r
tw_get_property(id = "Q180099", p = "P19")
#> [1] "Q1345"
```

which, as expected, will give me another wikidata id. But what does,
“Q1345” stand for? I should ask for its label.

``` r
tw_get_label(id = "Q1345")
#> [1] "Philadelphia"
```

Alright, I know Philadelphia, but if it was a smaller place, perhaps I’d
need to ask in which country it is located. So I would ask for the
correspondent property, P17.

``` r
tw_get_property(id = "Q1345", p = "P17")
#> [1] "Q30"
```

Oh, no, another Wikidata id\!

``` r
tw_get_label(id = "Q30")
#> [1] "United States of America"
```

It takes some time to get used, but I suppose you get the gist of it.

You can also pipe all of the above, like this:

``` r
tw_search(search = "Margaret Mead") %>% # search for Margeret Mead
  tw_filter_first(p = "P31", q = "Q5") %>% # keep only the first result that is of a human
  tw_get_property(p = "P19") %>% # ask for the place of birth
  tw_get_property(p = "P17") %>% # ask for the country where that place of birth is located
  tw_get_label() # ask what that id stands for
#> [1] "United States of America"
```

And here we are, we know in which country Margaret Mead was born.

This may seems complex, but can be actually quite useful to understand
how Wikidata works.

This functions can also be combined, for example, like this:

``` r
get_bio <- function(id, language = "en") {
  tibble::tibble(
    label = tw_get_label(id = id, language = language),
    description = tw_get_description(id = id, language = language),
    year_of_birth = tw_get_property(id = id, p = "P569") %>% 
      lubridate::ymd_hms() %>% 
      lubridate::year() %>%
      head(1), 
    year_of_death = tw_get_property(id = id, p = "P570") %>% 
      lubridate::ymd_hms() %>% 
      lubridate::year() %>% 
      head(1)
  )
}

tw_search(search = "Margaret Mead") %>% 
  tw_filter_first(p = "P31", q = "Q5") %>%
  get_bio()
#> # A tibble: 1 x 4
#>   label         description             year_of_birth year_of_death
#>   <chr>         <chr>                           <dbl>         <dbl>
#> 1 Margaret Mead American anthropologist          1901          1978
```

I can of course get the response in languages other than English, as
long as those are available on Wikidata.

``` r
tw_search(search = "Margaret Mead") %>% 
  tw_filter_first(p = "P31", q = "Q5") %>%
  get_bio(language = "it")
#> # A tibble: 1 x 4
#>   label         description              year_of_birth year_of_death
#>   <chr>         <chr>                            <dbl>         <dbl>
#> 1 Margaret Mead antropologa statunitense          1901          1978
```

## Queries

All of the above works similarly to how we often use websistes such as
Wikipedia, or search engines. Wikidata, however, has powerful tools for
complex queries. Think something like “give me all of these fields for
all items that have this value for this property, but not that other
value for that other property”.

To achieve this, you can run queries, following [instructions on
Wikidata.org](https://www.wikidata.org/wiki/Wikidata:SPARQL_query_service/queries/examples).
From R, you would run those using
`WikidataQueryServiceR::query_wikidata()`. This is powerful, but perhaps
somewhat intimidating for those who are less familiar with database
queries, SPARQL, and the likes.

`tidiwikidatar` does not currently plan to deal with complex queries.
However, at this stage it has a basic function, `tw_query`, which should
instantly make sense for R users.

Say, for example, you are interested in all women (P21 == Q6581072) who
are resistance fighters (P106 == Q6581072).

You can then make a data frame with two columns (p and q), and some
requirements, like this:

``` r
query_df <- tibble::tribble(~p, ~q, 
                            "P106", "Q1397808",
                            "P21", "Q6581072")

# if you prefer, you can input the same as a list, like this:
# query_l <- list(c(p = "P106", q = "Q1397808"),
#                c(p = "P21", q = "Q6581072"))

query_df
#> # A tibble: 2 x 2
#>   p     q       
#>   <chr> <chr>   
#> 1 P106  Q1397808
#> 2 P21   Q6581072
```

You can then pass it to `tw_query()`, and get a nicely formatted
dataframe with all female resistance fighters on Wikidata.

``` r
tw_query(query = query_df)
#> # A tibble: 639 x 3
#>    id     label                 description                                     
#>    <chr>  <chr>                 <chr>                                           
#>  1 Q66335 Traute Lafrenz        active within the White Rose non-violent resist…
#>  2 Q69042 Libertas Schulze-Boy… German opponent of the Nazis who belonged to th…
#>  3 Q69720 Tatiana von Metterni… German patron of the arts of Russian birth      
#>  4 Q70593 Cato Bontjes van Beek member of the German resistance to Nazism       
#>  5 Q70670 Elisabeth von Thadden German resistance member (1890-1944)            
#>  6 Q70772 Hilde Coppi           Antifascist member of resistance                
#>  7 Q70873 Inge Scholl           German activist                                 
#>  8 Q71688 Marie-Luise Jahn      German resistance fighter; active within the Wh…
#>  9 Q72908 Mildred Harnack       American-German literary historian, professor, …
#> 10 Q74405 Elisabeth Schumacher  German resistance member                        
#> # … with 629 more rows
```

You can also ask other fields, beyond label and description using the
field parameter of `tw_query()`. But for this readme, I’ll keep things
simple. Do you want more information about these results? You can still
use the same commands used above, e.g.

``` r
tibble::tribble(~p, ~q, 
                "P106", "Q1397808",
                "P21", "Q6581072"
) %>% 
  tw_query() %>% 
  dplyr::slice(1) %>% 
  get_bio()
#> Warning: All formats failed to parse. No formats found.
#> Warning: 1 failed to parse.
#> # A tibble: 1 x 4
#>   label               description         year_of_birth year_of_death
#>   <chr>               <chr>                       <dbl>         <dbl>
#> 1 Lady Ann Cunningham Scottish noblewoman            NA          1646
```

## Copyright and credits

This package has been created by [Giorgio
Comai](https://giorgiocomai.eu), data analyst and researcher at
[OBCT/CCI](https://balcanicaucaso.org/), within the scope of
[EDJNet](https://europeandatajournalism.eu/), the European Data
Journalism Network.

It is distributed under the GPL license.
