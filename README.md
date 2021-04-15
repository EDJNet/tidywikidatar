
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidywikidatar

<!-- badges: start -->

[![R-CMD-check](https://github.com/EDJNet/tidywikidatar/workflows/R-CMD-check/badge.svg)](https://github.com/EDJNet/tidywikidatar/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/tidywikidatar)](https://CRAN.R-project.org/package=tidywikidatar)
<!-- badges: end -->

The goal of `tidywikidatar` is to facilitate interaction with Wikidata:

  - all responses are transformed into data frames or simple character
    vectors
  - it is easy to enable efficient caching in a local sqlite database

If you want to benefit of the wealth of information stored by Wikidata,
but you do not like SPARQL queries and nested lists, then you may find
`tidywikidatar` useful. If you prefer working with nested lists and
SPARQL queries, or if you plan to build more complex queries, then you
should probably use
[`WikidataR`](https://github.com/Ironholds/WikidataR) or Wikimedia’s own
[`WikidataQueryServiceR`](https://github.com/wikimedia/WikidataQueryServiceR)
(under the hood, `tidywikidatar` is largely based on those packages).

## Installation

You can install `tidywikidatar` from
[Github](https://github.com/EDJNet/tidywikidatar) with:

``` r
# install.packages("remotes")
remotes::install_github("EDJNet/tidywikidatar")
```

## Before you start

This package assumes some familiarity with basic Wikidata concepts. For
reference, see [the introduction on the official
website](https://www.wikidata.org/wiki/Wikidata:Introduction).

At the most basic, you should know that every item in Wikidata has an id
(it always starts with a Q, something like `Q123456`). Each item is
described by properties (they always start with a P, something like
`P1234`).

So for example, if I am interested in the anthropologist Margaret Mead,
I will search her name on Wikidata and discover that she is
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

`tidywikidatar` makes it easy to cache locally responses (both searches
and details about specific items) in a sqlite database to reduce load on
Wikidata’s servers. These sqlite databases are by default stored in the
current working directory under a `tw_data` folder. It may be useful to
store them in a folder where they can be retrieved easily even when
working on different projects, but this is obviously a matter of
personal taste. You can enable caching for the current session with
`tw_enable_cache()` and set the cache folder to be used throughout a
session with `tw_set_cache_folder()`. The first lines of a script using
`tidywikidatar` would often look like this:

``` r
library("tidywikidatar")
tw_enable_cache()
tw_set_cache_folder(path = fs::path(fs::path_home_r(), "R", "tw_data"))
tw_create_cache_folder(ask = FALSE)
```

This also means that you can re-run code when offline, as data are
downloaded from Wikidata’s server only at first run (that is, unless you
set `cache = FALSE` or `overwrite_cache = TRUE` when calling the
respective functions, or disable caching for the current session with
`tw_disable_cache()`).

## Finding details about something

Most `tidywikidatar` functions are built around the idea that you know
what you are looking for, and just want to get what Wikidata knows about
it, assuming the preferred choice would be among the top results.

Let’s take this again from the beginning. As I mentioned, I am
interested in Margaret Mead, the famous pioneer anthropologist author of
“Coming of Age in Samoa”. This seems quite straightforward but there are
actually a number of things that are returned by searching for “Margaret
Mead” that are not the woman herself.

``` r
tw_search(search = "Margaret Mead")
#> # A tibble: 10 x 3
#>    id       label                               description                     
#>    <chr>    <chr>                               <chr>                           
#>  1 Q180099  Margaret Mead                       American anthropologist         
#>  2 Q810150… Margaret mead                       scientific article published on…
#>  3 Q667014… Margaret Mead                       scientific article published on…
#>  4 Q857246… Mead & Bateson                      business organisation           
#>  5 Q960776… Margaret Meadows                    <NA>                            
#>  6 Q762385… Margaret Meadowe                    Peerage person ID=628312        
#>  7 Q755066… Margaret Meadows                    Peerage person ID=183057        
#>  8 Q758123… Margaret Meade-Waldo                died 1954                       
#>  9 Q6759717 Margaret Mead Film Festival         annual film festival held in Ne…
#> 10 Q558970… Margaret Mead and Samoa: Coming of… <NA>
```

If I am running through a list of strings, and, for example, I am
actually interested in the most famous person by that name, I can filter
result by property, using the standard form. If, for example, I want
only the first result that is associated with “an instance of” (P31) -
“human” (Q5), I can run:

``` r
tw_search(search = "Margaret Mead") %>%
  tw_filter_first(p = "P31", q = "Q5")
#> # A tibble: 1 x 3
#>   id      label         description            
#>   <chr>   <chr>         <chr>                  
#> 1 Q180099 Margaret Mead American anthropologist
```

and, as expected, I get a single output: my beloved Margaret Mead.

Where was she born? I can ask directly for P19, place of birth:

``` r
tw_get_property(id = "Q180099", p = "P19")
#> # A tibble: 1 x 3
#>   id      property value
#>   <chr>   <chr>    <chr>
#> 1 Q180099 P19      Q1345
```

which, as expected, will give me another wikidata id. But what does,
“Q1345” stand for? I should ask for its label.

``` r
tw_get_label(id = "Q1345")
#> [1] "Philadelphia"
```

Alright, I know where Philadelphia, but if it was a smaller place,
perhaps I’d need to ask in which country it is located. So I would ask
for the correspondent property, P17.

``` r
tw_get_property(id = "Q1345", p = "P17")
#> # A tibble: 1 x 3
#>   id    property value
#>   <chr> <chr>    <chr>
#> 1 Q1345 P17      Q30
```

Oh, no, another Wikidata id\! That’s the way it works… let’s ask for its
label:

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
  dplyr::pull(value) %>% # take its result and
  tw_get_property(p = "P17") %>% # ask for the country where that place of birth is located
  tw_get_label() # ask what that id stands for
#> [1] "Philadelphia"
```

And here we are, we know in which country Margaret Mead was born.

The procedure above may seem a bit convoluted, but it is actually quite
representative of how Wikidata stores information.

As you would expect, such functions can also be combined, for example,
like this:

``` r
get_bio <- function(id, language = "en") {
  tibble::tibble(
    label = tw_get_label(id = id, language = language),
    description = tw_get_description(id = id, language = language),
    year_of_birth = tw_get_property(id = id, p = "P569") %>%
      dplyr::pull(value) %>%
      head(1) %>%
      lubridate::ymd_hms() %>%
      lubridate::year(),
    year_of_death = tw_get_property(id = id, p = "P570") %>%
      dplyr::pull(value) %>%
      head(1) %>%
      lubridate::ymd_hms() %>%
      lubridate::year()
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

## Serial operations

More examples regarding serial operations, and streamlined queries over
long lists of ids will be available in a dedicated vignette in a future
version.

In the meantime, let us just say that if we wanted to have a list of all
the “awards received”
([P166](https://www.wikidata.org/wiki/Property:P166)) by Margaret Mead,
and fellow anthropologists and folklorists Ruth Benedict and Zora Neale
Hurston, we can achieve that in a single call:

``` r

tw_get_property(
  id = c("Q180099", "Q228822", "Q220480"),
  p = "P166",
  language = "en"
) 
#> # A tibble: 14 x 3
#>    id      property value    
#>    <chr>   <chr>    <chr>    
#>  1 Q180099 P166     Q17144   
#>  2 Q180099 P166     Q782022  
#>  3 Q180099 P166     Q8017107 
#>  4 Q180099 P166     Q1967852 
#>  5 Q180099 P166     Q52382875
#>  6 Q228822 P166     Q1967852 
#>  7 Q228822 P166     Q52382875
#>  8 Q228822 P166     Q752297  
#>  9 Q220480 P166     Q1316544 
#> 10 Q220480 P166     Q1967852 
#> 11 Q220480 P166     Q5461701 
#> 12 Q220480 P166     Q5461189 
#> 13 Q220480 P166     Q4765305 
#> 14 Q220480 P166     Q1316544
```

Again, Wikidata ids. We can of course get their relative labels using
the functions outlined above, but `tidywikidatar` has a convenience
function - `tw_label()` that will achieve what you want in most such
cases.

``` r
tw_get_property(
  id = c("Q180099", "Q228822", "Q220480"),
  p = "P166",
  language = "en"
) %>% 
  tw_label()
#> # A tibble: 14 x 3
#>    id                property       value                                       
#>    <chr>             <chr>          <chr>                                       
#>  1 Margaret Mead     award received Presidential Medal of Freedom               
#>  2 Margaret Mead     award received Kalinga Prize                               
#>  3 Margaret Mead     award received William Procter Prize for Scientific Achiev…
#>  4 Margaret Mead     award received National Women's Hall of Fame               
#>  5 Margaret Mead     award received AAAS Fellow                                 
#>  6 Ruth Benedict     award received National Women's Hall of Fame               
#>  7 Ruth Benedict     award received AAAS Fellow                                 
#>  8 Ruth Benedict     award received Doctor of Philosophy                        
#>  9 Zora Neale Hurst… award received Guggenheim Fellowship                       
#> 10 Zora Neale Hurst… award received National Women's Hall of Fame               
#> 11 Zora Neale Hurst… award received Florida Women's Hall of Fame                
#> 12 Zora Neale Hurst… award received Florida Artists Hall of Fame                
#> 13 Zora Neale Hurst… award received Anisfield-Wolf Book Awards                  
#> 14 Zora Neale Hurst… award received Guggenheim Fellowship
```

## Qualifiers

In most cases, things are quite straightforward: each item has one or
more values for a given property.

However, some properties have additional qualifiers.

As an example, let’s look at someone whose life is seemlingly less
adventurous than that of Margaret Mead, but whose Wikidata page has
properties with a more interesting combination of qualifiers: the
current president of the European Parliament David Sassoli
([Q2391857](https://www.wikidata.org/wiki/Q2391857)).

If we look at his “positions held”
([P39](https://www.wikidata.org/wiki/Property:P39)), we find the
following:

``` r

purrr::map_chr(
  .x = tw_get_property(id = "Q2391857", p = "P39") %>% dplyr::pull(value),
  .f = tw_get_label
)
#> [1] "member of the European Parliament"   
#> [2] "President of the European Parliament"
#> [3] "member of the European Parliament"   
#> [4] "member of the European Parliament"
```

He has been more than once “member of the European Parliament”, and once
“President of the European Parliament”. But this is not all that
Wikidata knows about it: each of these properties comes with qualifiers.

``` r
qualifiers_df <- tw_get_qualifiers(id = "Q2391857", p = "P39")
qualifiers_df
#> # A tibble: 21 x 6
#>    id       property qualifier_id qualifier_property value                   set
#>    <chr>    <chr>    <chr>        <chr>              <chr>                 <int>
#>  1 Q2391857 P39      Q27169       P2937              Q17315694                 1
#>  2 Q2391857 P39      Q27169       P580               +2014-07-01T00:00:00Z     1
#>  3 Q2391857 P39      Q27169       P4100              Q507343                   1
#>  4 Q2391857 P39      Q27169       P768               Q3677909                  1
#>  5 Q2391857 P39      Q27169       P1268              Q47729                    1
#>  6 Q2391857 P39      Q27169       P2715              Q1376095                  1
#>  7 Q2391857 P39      Q740126      P580               +2019-07-03T00:00:00Z     2
#>  8 Q2391857 P39      Q740126      P1365              Q440710                   2
#>  9 Q2391857 P39      Q27169       P2937              Q4644021                  3
#> 10 Q2391857 P39      Q27169       P580               +2009-07-14T00:00:00Z     3
#> # … with 11 more rows
```

As usual, Wikidata presents everything as combinations of properties and
values. Let’s translate each of these to their respective label, and
separate each set of information we have about the “positions held” by
Mr. Sassoli:

``` r
qualifiers_labelled_df <- qualifiers_df %>%
  dplyr::transmute(
    who = tw_get_label(id = id, language = "en"),
    did = tw_get_property_label(property = property, language = "en"),
    what = tw_get_label(id = qualifier_id, language = "en"),
    how = tw_get_property_label(property = qualifier_property, language = "en"),
    value = purrr::map_chr(
      .x = value,
      .f = function(x) {
        if (stringr::str_starts(
          string = x,
          pattern = "Q"
        )) {
          tw_get_label(
            id = x,
            language = "en"
          )
        } else {
          stringr::str_extract(
            string = x,
            pattern = "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}"
          )
        }
      }
    ),
    set = set
  )

qualifiers_labelled_df %>%
  dplyr::group_by(set) %>%
  knitr::kable()
```

| who           | did           | what                                 | how                 | value                                            | set |
| :------------ | :------------ | :----------------------------------- | :------------------ | :----------------------------------------------- | --: |
| David Sassoli | position held | member of the European Parliament    | parliamentary term  | Eighth European Parliament                       |   1 |
| David Sassoli | position held | member of the European Parliament    | start time          | 2014-07-01                                       |   1 |
| David Sassoli | position held | member of the European Parliament    | parliamentary group | Progressive Alliance of Socialists and Democrats |   1 |
| David Sassoli | position held | member of the European Parliament    | electoral district  | Central Italy                                    |   1 |
| David Sassoli | position held | member of the European Parliament    | represents          | Democratic Party                                 |   1 |
| David Sassoli | position held | member of the European Parliament    | elected in          | 2014 European Parliament election                |   1 |
| David Sassoli | position held | President of the European Parliament | start time          | 2019-07-03                                       |   2 |
| David Sassoli | position held | President of the European Parliament | replaces            | Antonio Tajani                                   |   2 |
| David Sassoli | position held | member of the European Parliament    | parliamentary term  | Seventh European Parliament                      |   3 |
| David Sassoli | position held | member of the European Parliament    | start time          | 2009-07-14                                       |   3 |
| David Sassoli | position held | member of the European Parliament    | parliamentary group | Progressive Alliance of Socialists and Democrats |   3 |
| David Sassoli | position held | member of the European Parliament    | electoral district  | Central Italy                                    |   3 |
| David Sassoli | position held | member of the European Parliament    | represents          | Democratic Party                                 |   3 |
| David Sassoli | position held | member of the European Parliament    | elected in          | 2009 European Parliament election                |   3 |
| David Sassoli | position held | member of the European Parliament    | end time            | 2014-06-30                                       |   3 |
| David Sassoli | position held | member of the European Parliament    | parliamentary term  | Ninth European Parliament                        |   4 |
| David Sassoli | position held | member of the European Parliament    | start time          | 2019-07-02                                       |   4 |
| David Sassoli | position held | member of the European Parliament    | parliamentary group | Progressive Alliance of Socialists and Democrats |   4 |
| David Sassoli | position held | member of the European Parliament    | electoral district  | Italy                                            |   4 |
| David Sassoli | position held | member of the European Parliament    | represents          | Democratic Party                                 |   4 |
| David Sassoli | position held | member of the European Parliament    | elected in          | 2019 European Parliament election                |   4 |

That’s quite a lot of useful detail. The construction of the request can
be quite complicated, but keep in mind that if you do this
programmatically you will likely use this for filtering a specific piece
of information based on a combination of properties, and you will only
less frequently need to extract all available information.

Fundamentally, you won’t be touching anything that is not a vector or a
tidy data frame, which is ultimately a key goal of `tidywikidatar`: make
use of the wealth of information stored by Wikidata from R without
having to deal with either nested lists or SPARQL queries.

## Queries

All of the above works similarly to how we often use websistes such as
Wikipedia, or search engines: we search for something specific to find
information about it. Wikidata, however, has powerful tools for complex
queries. Think something like “give me all of these fields for all items
that have this value for this property, but not that other value for
that other property”.

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
query_df <- tibble::tribble(
  ~p, ~q,
  "P106", "Q1397808",
  "P21", "Q6581072"
)

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
dataframe with all women who are resistance fighters on Wikidata.

``` r
tw_query(query = query_df)
#> # A tibble: 651 x 3
#>    id      label            description                                         
#>    <chr>   <chr>            <chr>                                               
#>  1 Q304262 Hannie van Leeu… Dutch politician (1926-2018)                        
#>  2 Q324718 Martha Dodd      American spy for the Soviet Union                   
#>  3 Q354512 Adele Stürzl     Austrian politician, member of the Austrian resista…
#>  4 Q441439 Henriette Rolan… Dutch politician, editor (1869-1952)                
#>  5 Q443262 Lozen            Apache prophetess and warrior                       
#>  6 Q448486 Hannie Schaft    Dutch communist resistance fighter and martyr       
#>  7 Q451631 Françoise Rosay  actress (1891-1974)                                 
#>  8 Q452272 Charlotte Delbo  French writer and resistance fighter (1913-1985)    
#>  9 Q457505 Danielle Casano… French resistance member (1909-1943)                
#> 10 Q459656 Suzanne Spaak    Belgian anti-Nazi resistance worker and counterinte…
#> # … with 641 more rows
```

Or perhaps, you are interested only in women who are resistance fighters
who have “France” ([Q142](https://www.wikidata.org/wiki/Q142)) as
“country of citizenship”
([P27](https://www.wikidata.org/wiki/Property:P27))? And perhaps you
want the description in Italian, and if not available in French, and
only then look for other fallback options?

``` r
tibble::tribble(
  ~p, ~q,
  "P106", "Q1397808", # Occupation: resistance fighter
  "P21", "Q6581072", # Sex or gender: female
  "P27", "Q142"
) %>% # Country of citizenship: France
  tw_query(language = c("it", "fr"))
#> # A tibble: 101 x 3
#>    id      label                description                                     
#>    <chr>   <chr>                <chr>                                           
#>  1 Q270319 Christiane Desroche… egittologa e archeologa francese                
#>  2 Q283654 Marija Skobcova      suora e santa russa, vittima dell'Olocausto     
#>  3 Q35740… Yvette Farnoux       résistante française                            
#>  4 Q35741… Yvonne Abbas         résistante française                            
#>  5 Q26965… Yolande Beekman      espionne et agente secret des Special Operation…
#>  6 Q30097… Cécile Cerf          résistante française                            
#>  7 Q30812… Francine Fromond     <NA>                                            
#>  8 Q31324… Henriette Moriamé    <NA>                                            
#>  9 Q31760… Jeanne Gaillard      historienne et résistante française             
#> 10 Q31760… Jeanne Laurent       scrittrice francese                             
#> # … with 91 more rows
```

You can also ask other fields, beyond label and description, using the
`field` parameter of `tw_query()`. But for this readme, I’ll keep things
simple. Do you want more information about these results without
learning yet another set of Wikidata terminology? You can still use the
same commands described above, e.g.

``` r
tibble::tribble(
  ~p, ~q,
  "P106", "Q1397808",
  "P21", "Q6581072",
  "P27", "Q142"
) %>%
  tw_query() %>%
  dplyr::slice(1) %>%
  get_bio()
#> # A tibble: 1 x 4
#>   label                           description        year_of_birth year_of_death
#>   <chr>                           <chr>                      <dbl>         <dbl>
#> 1 Christiane Desroches Noblecourt French egyptologi…          1913          2011
```

Keep in mind that Wikidata queries are not cached locally.

## How caching works

`tidywikidatar` tries to reduce load on Wikidata’s server and speeding
up re-processing of scripts by caching data locally in sqlite databases.
They are stored locally in the folder defined by `tw_set_cache_folder()`
- by default, in the current working directory - when cache is enabled
(typically, with `tw_enable_cache()` at the beginning of a session).

To reduce the size of local files, if data are requested in a specific
language, then only data in that language are stored locally.

The easiest way to reset the cache is simply to delete the cache folder.

Results are stored in different databases by language, and function
used; `tw_search()`, `tw_get()`, and `tw_get_qualifiers()`, for example,
store data in different files.

`tw_query()` is never cached.

## Limitations

`tidywikidatar` strives to strike a balance between ease of use and full
access to information available on Wikidata. This means that, for
examples, dates are returned as simple text strings, without
accompanying details such as calendar (e.g. Julian or Gregorian) and
precision (e.g. precise just to the level of century). Some amounts are
returned as numeric strings, without the accompanying unit of
measurement. The user should be aware of such issues in their own use
cases, and consider using other packages if such matters are determinant
for them.

## Known issues

  - `tw_search()` always returns label and description in English (to be
    fixed)

## Copyright and credits

This package has been created by [Giorgio
Comai](https://giorgiocomai.eu), data analyst and researcher at
[OBCT/CCI](https://balcanicaucaso.org/), within the scope of
[EDJNet](https://europeandatajournalism.eu/), the European Data
Journalism Network.

It is distributed under the MIT license.
