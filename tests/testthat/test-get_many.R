test_that("check if labels are returned correctly when more than one id given", {
  expect_equal(object = {
    tw_get_label(
      id = c(
        "Q180099",
        "Q228822"
      ),
      language = "en"
    )
  }, expected = c(
    "Margaret Mead",
    "Ruth Benedict"
  ))
})


test_that("check if labels are returned more efficiently when same id given", {
  expect_lt(object = {
    tw_get_label(
      id = c(
        "Q180099"
      ),
      language = "en"
    )
    before <- Sys.time()
    tw_get_label(
      id = rep(x = "Q180099", 10),
      language = "en"
    )
    after <- Sys.time()
    after - before
  }, expected = {
    before <- Sys.time()
    purrr::map_chr(.x = rep(x = "Q180099", 10),
                   .f = function(x) {
      tw_get_label(
        id = x,
        language = "en"
      )
    })

    after <- Sys.time()
    after - before
  })
})

test_that("check if descriptions are returned more efficiently when same id given", {
  expect_lt(object = {
    tw_get_description(
      id = "Q180099",
      language = "en"
    )
    before <- Sys.time()
    tw_get_description(
      id = rep(x = "Q180099", 10),
      language = "en"
    )
    after <- Sys.time()
    after - before
  }, expected = {
    before <- Sys.time()

    purrr::map_chr(.x = rep(x = "Q180099", 10), .f = function(x) {
      tw_get_description(
        id = x,
        language = "en"
      )
    })

    after <- Sys.time()
    after - before
  })
})

test_that("check if property labels are returned more efficiently when same id given", {
  expect_lt(object = {
    tw_get_property_label(
      property = c(
        "P31"
      ),
      language = "en"
    )

    before <- Sys.time()
    tw_get_property_label(
      property = c(
        "P31",
        "P31",
        "P31"
      ),
      language = "en"
    )
    after <- Sys.time()
    after - before
  }, expected = {
    before <- Sys.time()

    purrr::map_chr(.x = c(
      "P31",
      "P31",
      "P31"
    ), .f = function(x) {
      tw_get_property_label(
        property = x,
        language = "en"
      )
    })
    after <- Sys.time()
    after - before
  })
})
